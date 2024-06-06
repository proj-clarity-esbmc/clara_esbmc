#ifndef CLARITY_FRONTEND_CLARITY_AST_LANGUAGE_H_
#define CLARITY_FRONTEND_CLARITY_AST_LANGUAGE_H_

#include <clang-c-frontend/clang_c_language.h>
#include <util/language.h>
#include <fstream>
#include <sstream>
#include <nlohmann/json.hpp>
#include <boost/filesystem.hpp>

class clarity_languaget : public clang_c_languaget
{
public:
  clarity_languaget();

  bool parse(const std::string &path) override;

  bool final(contextt &context) override;

  bool typecheck(contextt &context, const std::string &module) override;

  std::string id() const override
  {
    return "clarity_ast";
  }

  void show_parse(std::ostream &out) override;

  // temp file used by clang-c-frontend
  std::string temp_path;

  // Functions to handle temp C file used by clang-c-frontend
  std::string get_temp_file();
  std::string temp_c_file();

  languaget *new_language() const override
  {
    return new clarity_languaget;
  }

  bool convert_intrinsics(contextt &context);

  // function name for verification that requires this information before GOTO conversion phase.
  std::string func_name;

  // smart contract source
  std::string smart_contract;

  // store AST json in nlohmann::json data structure
  nlohmann::json src_ast_json;
  nlohmann::json intrinsic_json;

  languaget *clang_c_module;
};

languaget *new_clarity_language();

#endif /* CLARITY_FRONTEND_CLARITY_AST_LANGUAGE_H_ */
