#include <util/compiler_defs.h>
// Remove warnings from Clang headers
CC_DIAGNOSTIC_PUSH()
CC_DIAGNOSTIC_IGNORE_LLVM_CHECKS()
#include <clang/Frontend/ASTUnit.h>
CC_DIAGNOSTIC_POP()

#include <clarity-frontend/clarity_language.h>
// TODO #include <clarity-frontend/clarity_convert.h>
#include <clarity-frontend/clarity_template.h>
#include <clang-c-frontend/clang_c_main.h>
#include <clang-cpp-frontend/clang_cpp_adjust.h>
#include <clang-c-frontend/clang_c_convert.h>
#include <c2goto/cprover_library.h>
#include <util/c_link.h>

languaget *new_clarity_language()
{
  return new clarity_languaget;
}

clarity_languaget::clarity_languaget()
{
  std::string fun = config.options.get_option("function");
  if (!fun.empty())
    func_name = fun;

  std::string sol = config.options.get_option("sol");
  if (sol.empty())
  {
    log_error("Please set the smart contract source file via --sol");
    abort();
  }
  smart_contract = sol;
}

std::string clarity_languaget::get_temp_file()
{
  // Create a temp file for clang-tool
  // needed to convert intrinsics
  auto p = boost::filesystem::temp_directory_path();
  if (!boost::filesystem::exists(p) || !boost::filesystem::is_directory(p))
  {
    log_error("Can't find temporary directory (needed to convert intrinsics)");
    abort();
  }

  // Create temporary directory
  p += "/esbmc_clarity_temp";
  boost::filesystem::create_directory(p);
  if (!boost::filesystem::is_directory(p))
  {
    log_error(
      "Can't create temporary directory (needed to convert intrinsics)");
    abort();
  }

  // populate temp file
  std::ofstream f;
  p += "/temp_clar.c";
  f.open(p.string());
  f << temp_c_file();
  f.close();

  return p.string();
}

bool clarity_languaget::parse(const std::string &path)
{
  // prepare temp file
  temp_path = get_temp_file();

  // get AST nodes of ESBMC intrinsics and the dummy main
  // populate ASTs inherited from parent class
  auto clar_lang = std::exchange(config.language, {language_idt::C, ""});
  clang_c_languaget::parse(temp_path);
  config.language = std::move(clar_lang);

  // Process AST json file
  std::ifstream ast_json_file_stream(path);
  std::string new_line, ast_json_content;

  while (getline(ast_json_file_stream, new_line))
  {
    if (new_line.find(".sol =======") != std::string::npos)
    {
      break;
    }
  }
  while (getline(ast_json_file_stream, new_line))
  {
    // file pointer continues from "=== *.sol ==="
    if (new_line.find(".sol =======") == std::string::npos)
    {
      ast_json_content = ast_json_content + new_line + "\n";
    }
    else
    {
      assert(!"Unsupported feature: found multiple contracts defined in a single .sol file");
    }
  }

  // parse explicitly
  src_ast_json = nlohmann::json::parse(ast_json_content);

  return false;
}

bool clarity_languaget::convert_intrinsics(contextt &context)
{
  clang_c_convertert converter(context, ASTs, "C++");
  if (converter.convert())
    return true;

  return false;
}

bool clarity_languaget::typecheck(contextt &context, const std::string &module)
{
  contextt new_context;
  convert_intrinsics(
    new_context); // Add ESBMC and TACAS intrinsic symbols to the context

#if 0
  TODO

  clarity_convertert converter(
    new_context, src_ast_json, func_name, smart_contract);
  if (converter.convert()) // Add Clarity symbols to the context
    return true;
#endif

  // migrate from clang_c_adjust to clang_cpp_adjust
  // for the reason that we need clang_cpp_adjust::adjust_side_effect
  // to adjust the created temporary object
  // otherwise it would raise "unknown side effect: temporary_object"
  clang_cpp_adjust adjuster(new_context);
  if (adjuster.adjust())
    return true;

  if (c_link(
        context,
        new_context,
        module)) // also populates language_uit::context
    return true;

  return false;
}

void clarity_languaget::show_parse(std::ostream &)
{
  assert(!"come back and continue - clarity_languaget::show_parse");
}

bool clarity_languaget::final(contextt &context)
{
  add_cprover_library(context);
  clang_c_maint c_main(context);
  return c_main.clang_main();
}

std::string clarity_languaget::temp_c_file()
{
  // This function populates the temp file so that Clang has a compilation job.
  // Clang needs a job to convert the intrinsics.
  std::string content =
    R"(int main() { return 0; } )" + ClarityTemplate::clar_library;
  return content;
}
