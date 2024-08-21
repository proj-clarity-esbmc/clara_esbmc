#include <clarity-frontend/clarity_convert.h>
#include <clarity-frontend/clarity_template.h>
#include <clarity-frontend/typecast.h>
#include <util/arith_tools.h>
#include <util/bitvector.h>
#include <util/c_types.h>
#include <util/expr_util.h>
#include <util/i2string.h>
#include <util/mp_arith.h>
#include <util/std_expr.h>
#include <util/message.h>
#include <regex>

#include <fstream>
#include <iostream>
#include "clarity_convert.h"

clarity_convertert::clarity_convertert(
  contextt &_context,
  nlohmann::json &_ast_json,
  const std::string &_clar_func,
  const std::string &_contract_path)
  : context(_context),
    ns(context),
    src_ast_json(_ast_json),
    clar_func(_clar_func),
    contract_path(_contract_path),
    global_scope_id(0),
    current_scope_var_num(1),
    current_functionDecl(nullptr),
    current_forStmt(nullptr),
    current_functionName(""),
    current_contractName(""),
    scope_map({}),
    tgt_func(config.options.get_option("function")),
    tgt_cnt(config.options.get_option("clar_contract"))
{
  std::ifstream in(_contract_path);
  contract_contents.assign(
    (std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
}

// takes first param as input ast_node, the second param is output param where the node represents the objtype node inside an Ast_node
void clarity_convertert::get_objtype_node(
  const nlohmann::json &ast_node,
  nlohmann::json &objtype_node)
{
  objtype_node = ast_node[1]["objtype"];
}
// takes objtype node as input param
std::string
clarity_convertert::get_objtype_type_name(const nlohmann::json &objtype_node)
{
  return objtype_node[0];
}
std::string clarity_convertert::get_objtype_type_identifier(
  const nlohmann::json &objtype_node)
{
  return objtype_node[1];
}
std::string
clarity_convertert::get_objtype_type_size(const nlohmann::json &objtype_node)
{
  return objtype_node[2];
}

bool clarity_convertert::process_define_data_var(nlohmann::json &ast_node)
{
  ast_node[0]["nodeType"] = "VariableDeclaration";

  return false;
}

bool clarity_convertert::process_define_constant(nlohmann::json &ast_node)
{
  /* CASE 1 : 
   {    "identifier": "fixed",
        "id": 250,
        "span": {
          "start_line": 38,
          "start_column": 18,
          "end_line": 38,
          "end_column": 22
        },
        "value": "fixed constant value"
      }*/

  std::string identifier_name = ast_node[1]["identifier"];
  int id = ast_node[1]["id"];
  int start_line = ast_node[1]["span"]["start_line"];
  int start_column = ast_node[1]["span"]["start_column"];
  std::string identifier_value = "";
  std::string type_name = std::string(ast_node[1]["value"].type_name());

  // insert a node type information to be used later in get_expression_t()

  ast_node[1]["nodeType"] = "VariableDeclaration";
  // we will add expression type depending on the value node rules in each of the
  // following if cases

  if (type_name == "string")
  {
    ast_node[1]["expressionType"] = "Literal";
    identifier_value = ast_node[1]["value"];
    int type_size = 128;
    // "u10" for uint 10, 10 for int value of 10.
    if (type_name.find("u"))
      //type_name = "uint";
      type_name = "uint_const";
    else
      type_name = "int";

    //insert_dummy_objType(ast_node, type_name , type_size);
  }
  else if (type_name == "number")
  {
    ast_node[1]["expressionType"] = "Literal";
    identifier_value = std::to_string(ast_node[1]["value"].get<double>());
    // the type could be uint , int , double, float.
    // 128 represents 128-bit
    //insert_dummy_objType(ast_node, "uint" , 128);
  }
  else if (type_name == "array")
  {
    std::cout << " It's a list, so probably a function / operator \n";
  }
  else if (type_name == "object")
  {
    std::cout << " It's an object, so probably contains lit_ascii \n";
  }
  else
  {
    identifier_value = "invalid value";
  }

  std::cout << "Constant Definition"
            << " of : " << identifier_name << " | value : " << identifier_value
            << " found @ " << start_line << ":" << start_column << "\n";

  return false;
}

bool clarity_convertert::process_define_map(nlohmann::json &ast_node)
{
  return false;
}

bool clarity_convertert::process_expr_node(nlohmann::json &ast_node)
{
  auto expr = ast_node;
  // process expr node;
  if (expr["expr"].contains("List"))
  {
    // process expression
    auto vec_expr_list = expr["expr"]["List"];

    // process list
    for (auto &list_expr : vec_expr_list)
    {
      // process each list item
      if (list_expr.contains("expr"))
      {
        auto t_expr = list_expr["expr"];

        if (t_expr.contains("Atom"))
        {
          // id will always be an integer.
          int node_id = list_expr["id"];
          int start_line = list_expr["span"]["start_line"];
          int start_column = list_expr["span"]["start_column"];
          std::string t_expr_type = t_expr["Atom"];
          std::cout << "Expression of type : " << t_expr_type << " found @ "
                    << start_line << ":" << start_column << "\n";
        }
        else if (t_expr.contains("LiteralValue"))
        {
          auto t_expr_literalValue = t_expr["LiteralValue"];
          std::string t_expr_d_type;  // = t_expr_literalValue.first;
          std::string t_expr_d_value; // = t_expr_literalValue.second;
          for (auto &[key, value] : t_expr_literalValue.items())
          {
            if (key == "Principal")
            {
              t_expr_d_type = "Principal_";
              auto t_principal = t_expr_literalValue["Principal"];
              for (auto &[principal_key, principal_value] : t_principal.items())
              {
                t_expr_d_type += principal_key;
                t_expr_d_value = "STN" + principal_key;
              }
            }
            else
            {
              t_expr_d_type = key;
              t_expr_d_value = value;
            }
            std::cout << "Literal Value : Type " << key << " Value " << value
                      << "\n";
          }
        }
      }
    }
    std::cout << "\n--------- \n";
  }
  else
  {
    std::cout << "Incomplete expression \n";
  }
  return false;
}

void clarity_convertert::convert_dummy_uint_literal()
{
  typet symbolType = unsignedbv_typet(128);
  std::string varName = "myVar";
  std::string varId = "clar:@C@basicDummy@" + varName + "#123";

  locationt locationBegin;
  locationBegin.set_line("293");
  locationBegin.set_file("basicDummy.clar");

  std::string debugModuleName = locationBegin.file().as_string();

  symbolt symbol;
  get_default_symbol(
    symbol, debugModuleName, symbolType, varName, varId, locationBegin);

  bool is_state_var = true;

  symbol.lvalue = true;
  symbol.static_lifetime = is_state_var;
  symbol.file_local = !is_state_var;
  symbol.is_extern = false;

  symbolt &addedSymbol = *move_symbol_to_context(symbol);

  // unsigned int constant
  exprt val;
  convert_unsigned_integer_literal_with_type(symbolType, "23", val);

  //   BigInt const_value = string2integer("23");
  //   /*

  //     constant_exprt constant(
  //     integer2binary(z_ext_value, 32), // Convert the value to its binary representation
  //     integer2string(z_ext_value),     // Human-readable string representation of the value
  //     type                             // The type of the constant
  // );
  // */
  //   val = constant_exprt(integer2binary(const_value,128),integer2string(const_value),symbolType);
  clarity_gen_typecast(ns, val, symbolType);
  addedSymbol.value = val;

  {
    // Map the following equation : myResult = 23 + 50

    locationt location_begin;
    location_begin.set_line("393");
    location_begin.set_file("basicDummy.clar");
    std::string resultvar_name = "myResult";
    std::string resultvar_id = "clar:@C@basicDummy@" + resultvar_name + "#333";

    symbolt result_symbol;

    get_default_symbol(
      result_symbol,
      "basicDummy.clar",
      signedbv_typet(128),
      resultvar_name,
      resultvar_id,
      location_begin);

    result_symbol.lvalue = true;
    result_symbol.static_lifetime = true;
    result_symbol.file_local = false;
    result_symbol.is_extern = false;

    symbolt &addedSymbol = *move_symbol_to_context(result_symbol);
    BigInt x = 23;
    BigInt y = 50;
    constant_exprt arg1 = constant_exprt(x, signedbv_typet(128));
    constant_exprt arg2 = constant_exprt(y, signedbv_typet(128));

    plus_exprt addition(arg1, arg2);

    clarity_gen_typecast(ns, addition, result_symbol.type);
    addedSymbol.value = addition;
  }

  {
    ///////////////
    // Map the following : variable_sum = myVar + 50
    // myVar already exists in the symbol table
    ////////////

    // create a symbol to hold LHS

    symbolt varSumSymbol;
    typet varSumType = unsignedbv_typet(128);
    // a symbol needs the following attribs to be filled
    /* 
        
        std::string module_name,  //for debug only
        typet type, 
        std::string name,
        std::string id,
        locationt location)
        */

    std::string varSumName = "varSum";
    std::string varSumId =
      "clar:@C@" + current_contractName + "@" + varSumName + "#123";
    locationt location_begin;
    location_begin.set_line("393");
    location_begin.set_file("basicDummy.clar");

    get_default_symbol(
      varSumSymbol,
      "basicDummy.clar",
      varSumType,
      varSumName,
      varSumId,
      location_begin);

    varSumSymbol.lvalue = true;
    varSumSymbol.static_lifetime = true;
    varSumSymbol.file_local = false;
    varSumSymbol.is_extern = false;

    symbolt &addedSymbol = *move_symbol_to_context(varSumSymbol);

    //for RHS we need to create an expression
    // the expression is a binary expression
    // the first operand is a symbol_exprt
    // the second operand is a constant_exprt
    // the operation is a plus_exprt

    // first, look for the symbol in the symbol table as myVar has been created before
    // if not found, create a new symbol for myVar

    symbolt *myVarSymbol = context.find_symbol(varId);

    // if myVarSymbol is not found, create a new symbol for myVar
    // skipping this step for now. assuming myVar is already in the symbol table

    // create a symbol_exprt for myVar
    symbol_exprt myVarExpr(myVarSymbol->name, varSumType);
    myVarExpr.identifier(myVarSymbol->id);

    // create a constant_exprt for 50
    constant_exprt fifty(50, varSumType);

    // write code to use a symbol already in the symbol table to a plus expression

    // create a plus_exprt for myVar + 50
    plus_exprt sum(myVarExpr, fifty);

    // typecast the expression
    clarity_gen_typecast(ns, sum, varSumSymbol.type);

    // assign the expression to the symbol
    addedSymbol.value = sum;
  }

  {
    /*
        DEFINING STRUCTS AND USING THEM FOR SYMBOL TYPES

        Translate the following :
        
        typedef struct 
        {
          std::string name;
          std::string homeTeam;
        } principal;

        principal myPrincipal;
        
      */

    // Create a struct_typet for the principal struct
    struct_typet principal_type;
    std::string struct_name = "principal";
    std::string struct_id =
      prefix + "struct " + struct_name; //tag-struct principal
    principal_type.tag("struct " + struct_name);

    // Create a symbol for the principal variable
    symbolt principal_symbol;

    locationt location_begin;
    location_begin.set_line("444");
    location_begin.set_file("basicDummy.clar");

    get_default_symbol(
      principal_symbol,
      "basicDummy.clar",
      principal_type,
      struct_name,
      struct_id,
      location_begin);

    principal_symbol.is_type = true;
    //principal_symbol.static_lifetime = true;

    // Add the principal symbol to the symbol table
    symbolt &addedSymbol = *move_symbol_to_context(principal_symbol);

    // populate the struct with fields

    //define typet to represent strings
    typet string_type = array_typet(
      char_type(), from_integer(30, size_type())); // Array of 30 chars

    // Create component types for the name and homeTeam fields
    {
      struct_typet::componentt name_field;
      std::string name_field_name = "name";
      std::string name_field_id = "clar:@C@" + current_contractName + "@" +
                                  struct_name + "@" + name_field_name + "#123";

      //exprt new_expr1 ("symbol",string_type);
      symbol_exprt new_expr1(name_field_name, string_type);
      //new_expr1.identifier(name_field_id);
      new_expr1.identifier(name_field_id);
      new_expr1.name(name_field_name);
      new_expr1.cmt_lvalue(true);
      new_expr1.pretty_name(name_field_name);
      name_field.swap(new_expr1);

      name_field.id("component");
      //name_field.type().set("#member_name",name_field_name);    // this is just to represent it in a pretty way. ot has no other implication

      principal_type.components().push_back(name_field);
    }

    {
      struct_typet::componentt home_team;
      std::string home_team_name = "homeTeam";
      std::string home_team_id = "clar:@C@" + current_contractName + "@" +
                                 struct_name + "@" + home_team_name + "#123";

      //exprt new_expr1 ("symbol",string_type);
      symbol_exprt new_expr1(home_team_name, string_type);
      //new_expr1.identifier(home_team_id);
      new_expr1.identifier(home_team_id);
      new_expr1.name(home_team_name);
      new_expr1.cmt_lvalue(true);
      new_expr1.pretty_name(home_team_name);
      home_team.swap(new_expr1);

      home_team.id("component");
      //home_team.type().set("#member_name",home_team_name);    // this is just to represent it in a pretty way. ot has no other implication

      principal_type.components().push_back(home_team);
    }

    principal_type.location() = location_begin;
    addedSymbol.type = principal_type;

    //std::cout <<"Name field " <<principal_type.pretty() <<"\n";

    // create an instane of the struct principal
    {
      symbolt principal_instance;
      std::string principal_instance_name = "myPrincipal";
      std::string principal_instance_id = "clar:@C@" + current_contractName +
                                          "@" + principal_instance_name +
                                          "#123";
      std::string principal_struct_id =
        "tag-struct principal"; //prefix + "struct " + struct_name
      typet principal_type;

      if (context.find_symbol(principal_struct_id) != nullptr)
      {
        const symbolt *symbol = context.find_symbol(principal_struct_id);
        principal_type = symbol->type;
      }

      else
        abort();

      locationt principal_instance_location;
      principal_instance_location.set_line("555");
      principal_instance_location.set_file("basicDummy.clar");

      get_default_symbol(
        principal_instance,
        "basicDummy.clar",
        principal_type,
        principal_instance_name,
        principal_instance_id,
        principal_instance_location);

      principal_instance.lvalue = true;
      principal_instance.static_lifetime = true;
      principal_instance.file_local = false;
      principal_instance.is_extern = false;

      symbolt &added_principal_instance =
        *move_symbol_to_context(principal_instance);
    }
  }
}

void clarity_convertert::convert_dummy_string_literal()
{
  // a string literal in ESBMC is represented as a character array.
  // a character is an 8-bit value (unsigned 8 bit value).
  // in ESBMC arrays are represented as array_typet.
  // array_type has a subtype of char (unsignedbv(8)).
  std::string literalValue = "Ali";
  typet subType = signed_char_type();
  typet symbolType =
    array_typet(subType, from_integer(literalValue.length() + 1, size_type()));
  std::string varName = "myString";
  std::string varId = "clar:@C@basicDummy@" + varName + "#123";

  locationt locationBegin;
  locationBegin.set_line("293");
  locationBegin.set_file("basicDummy.clar");

  std::string debugModuleName = locationBegin.file().as_string();

  symbolt symbol;
  get_default_symbol(
    symbol, debugModuleName, symbolType, varName, varId, locationBegin);
  bool is_state_var = true;

  symbol.lvalue = true;
  symbol.static_lifetime = is_state_var;
  symbol.file_local = !is_state_var;
  symbol.is_extern = false;

  symbolt &addedSymbol = *move_symbol_to_context(symbol);

  exprt val;
  convert_string_literal(literalValue, val);

  clarity_gen_typecast(ns, val, symbolType);
  addedSymbol.value = val;
}

#if 0
void clarity_convertert::add_function_definition_symboltable()
{

  // create a function symbol


  //add statements to the function

  // add the function to the symbol table

  // add the function to the context


  codet func_body, while_body;
  static_lifetime_init(context, while_body);
  static_lifetime_init(context, func_body);

  while_body.make_block();
  func_body.make_block();

  // 1. get function call


    // 1.1 get contract symbol ("tag-contractName")
    std::string c_name = current_contractName;;
    const std::string id = prefix + c_name;
    

    // 1.2 construct a constructor call and move to func_body
    const std::string ctor_id = get_ctor_call_id(c_name);

   
    const symbolt &constructor ;
    code_function_callt call;
    call.location() = constructor.location;
    call.function() = symbol_expr(constructor);
    const code_typet::argumentst &arguments =
      to_code_type(constructor.type).arguments();
    call.arguments().resize(
      arguments.size(), static_cast<const exprt &>(get_nil_irep()));

    // move to "clar_main" body
    func_body.move_to_operands(call);

    // 2. construct a while-loop and move to func_body

    // 2.0 check visibility setting
    bool skip_vis =
      config.options.get_option("no-visibility").empty() ? false : true;
    if (skip_vis)
    {
      log_warning(
        "force to verify every function, even it's an unreachable "
        "internal/private function. This might lead to false positives.");
    }

    // 2.1 construct ifthenelse statement
    const struct_typet::componentst &methods =
      to_struct_type(contract.type).methods();
    bool is_tgt_cnt = c_name == contractName ? true : false;

    for (const auto &method : methods)
    {
      // we only handle public (and external) function
      // as the private and internal function cannot be directly called
      if (is_tgt_cnt)
      {
        if (
          !skip_vis && method.get_access().as_string() != "public" &&
          method.get_access().as_string() != "external")
          continue;
      }
      else
      {
        // this means functions inherited from base contracts
        if (!skip_vis && method.get_access().as_string() != "public")
          continue;
      }

      // skip constructor
      const std::string func_id = method.identifier().as_string();
      if (func_id == ctor_id)
        continue;

      // guard: nondet_bool()
      if (context.find_symbol("c:@F@nondet_bool") == nullptr)
        return true;
      const symbolt &guard = *context.find_symbol("c:@F@nondet_bool");

      side_effect_expr_function_callt guard_expr;
      guard_expr.name("nondet_bool");
      guard_expr.identifier("c:@F@nondet_bool");
      guard_expr.location() = guard.location;
      guard_expr.cmt_lvalue(true);
      guard_expr.function() = symbol_expr(guard);

      // then: function_call
      if (context.find_symbol(func_id) == nullptr)
        return true;
      const symbolt &func = *context.find_symbol(func_id);
      code_function_callt then_expr;
      then_expr.location() = func.location;
      then_expr.function() = symbol_expr(func);
      const code_typet::argumentst &arguments =
        to_code_type(func.type).arguments();
      then_expr.arguments().resize(
        arguments.size(), static_cast<const exprt &>(get_nil_irep()));

      // ifthenelse-statement:
      codet if_expr("ifthenelse");
      if_expr.copy_to_operands(guard_expr, then_expr);

      // move to while-loop body
      while_body.move_to_operands(if_expr);
    }
  // while-cond:
  // const symbolt &guard = *context.find_symbol("c:@F@nondet_bool");
  // side_effect_expr_function_callt cond_expr;
  // cond_expr.name("nondet_bool");
  // cond_expr.identifier("c:@F@nondet_bool");
  // cond_expr.cmt_lvalue(true);
  // cond_expr.location() = func_body.location();
  // cond_expr.function() = symbol_expr(guard);

  // // while-loop statement:
  // code_whilet code_while;
  // code_while.cond() = cond_expr;
  // code_while.body() = while_body;

  // // move to "clar_main"
  // func_body.move_to_operands(code_while);

  // 3. add "clar_main" to symbol table
  symbolt new_symbol;
  code_typet main_type;
  main_type.return_type() = empty_typet();
  const std::string clar_name = "clar_main_" + contractName;
  const std::string clar_id = "clar:@C@" + contractName + "@F@" + clar_name;
  const symbolt &contract = *context.find_symbol(prefix + contractName);
  new_symbol.location = contract.location;
  std::string debug_modulename =
    get_modulename_from_path(contract.location.file().as_string());
  get_default_symbol(
    new_symbol,
    debug_modulename,
    main_type,
    clar_name,
    clar_id,
    contract.location);

  new_symbol.lvalue = true;
  new_symbol.is_extern = false;
  new_symbol.file_local = false;

  symbolt &added_symbol = *context.move_symbol_to_context(new_symbol);

  // no params
  main_type.make_ellipsis();

  added_symbol.type = main_type;
  added_symbol.value = func_body;

  // 4. set "clar_main" as main function
  // this will be overwrite in multi-contract mode.
  config.main = clar_name;

  return false;

}
#endif

void clarity_convertert::add_dummy_builtin_functionCall()
{
  std::string literalA = "My name ";
  std::string literalB = "Fatima ";
  BigInt outputSize = literalA.length() + literalB.length() + 1;

  exprt functionExpression;
  std::string id_var = "c:@string_concat";
  std::string id_func = "c:@F@string_concat";

  typet subType = signed_char_type();
  typet symbolType =
    array_typet(subType, from_integer(outputSize, size_type()));
  std::string varName = "myConcat";
  std::string varId = "clar:@C@basicDummy@" + varName + "#123";

  locationt locationBegin;
  locationBegin.set_line("293");
  locationBegin.set_file("basicDummy.clar");

  std::string debugModuleName = locationBegin.file().as_string();

  symbolt symbol;
  get_default_symbol(
    symbol, debugModuleName, symbolType, varName, varId, locationBegin);

  bool is_state_var = true;

  symbol.lvalue = true;
  symbol.static_lifetime = is_state_var;
  symbol.file_local = !is_state_var;
  symbol.is_extern = false;

  symbolt &addedSymbol = *move_symbol_to_context(symbol);

  //check if it's already in the symbol table.
  // it should be, as all builtin functions and blockchain variables
  // have been pre-defined and added to symbol table during temp_file() creation

  // 1 - look for the function name in symbol table.

  // for global blockchain vars like block_height
  if (context.find_symbol(id_var) != nullptr)
  {
    symbolt &sym = *context.find_symbol(id_var);

    if (sym.value.is_empty() || sym.value.is_zero())
    {
      // update: set the value to rand (default 0）
      // since all the current support built-in vars are uint type.
      // we just set the value to c:@F@nondet_uint
      symbolt &r = *context.find_symbol("c:@F@nondet_uint");
      sym.value = r.value;
    }
    functionExpression = symbol_expr(sym);
  }

  // for builtin functions like concat.
  else if (context.find_symbol(id_func) != nullptr)
    functionExpression = symbol_expr(*context.find_symbol(id_func));
  else
  {
    std::cout << "ALL HELL BROKE LOSE\n";
    abort();
  }

  // ToDo: check validity of functionExpression first.
  if (functionExpression.is_nil())
  {
    std::cout << "Function expression is nil \n";
    abort();
  }

  // get type of expression: for a function call, the type is the return type
  //a functioncall is a code_type expression
  //a function body is a code_t type expression
  // not to be confused.

  typet returnValSymbolType =
    to_code_type(functionExpression.type()).return_type();

  side_effect_expr_function_callt call;
  call.function() = functionExpression;
  call.type() = returnValSymbolType;

  // populate params

  //find the number of arguments defined in the template (clarity_template.h)

  size_t no_of_arguments =
    to_code_type(functionExpression.type()).arguments().size();

  // since this is a test dummy. i am assuming just two fixed params to string_concat function
  exprt arg1, arg2;

  // convert string literals to exprt nodes which are irept nodes.
  convert_string_literal(literalA, arg1);
  convert_string_literal(literalB, arg2);

  call.arguments().push_back(arg1);
  call.arguments().push_back(arg2);

  // std::cout <<"Call node \n";
  // std::cout <<call.pretty(4);
  functionExpression = call;

  // std::cout <<"Functionexpression node \n";
  // std::cout <<functionExpression.pretty(4);

  clarity_gen_typecast(ns, functionExpression, symbolType);
  addedSymbol.value = functionExpression;
  if (addedSymbol.value.is_nil())
  {
    std::cout << "Added symbol value is nil\n";
  }
  else
  {
    std::cout << "Added symbol value is assigned\n";
  }
}
void clarity_convertert::add_dummy_symbol()
{
  bool experimental = true;
  if (experimental)
  {
//translate : define-constant myvar u23
//task : create a dummy symbol of 128 bits : unsigned
// add reference to that diagram which states all the members of a symbol

/*
        typet type;
        exprt value;
        locationt location;
        irep_idt id;
        irep_idt module;
        irep_idt name;
        irep_idt mode;
      
      */
#define STRING_CONSTANT_ENABLED
#define UNSIGNED_INT_CONSTANT_ENABLED
    //#define BUILTIN_FUNCTIONCALL_ENABLED
    //#define PUBLIC_FUNCTION_ENABLED

#ifdef UNSIGNED_INT_CONSTANT_ENABLED
    convert_dummy_uint_literal();
#endif

#ifdef STRING_CONSTANT_ENABLED
    convert_dummy_string_literal();
#endif

#ifdef BUILTIN_FUNCTIONCALL_ENABLED
    add_dummy_builtin_functionCall();
#endif

#ifdef PUBLIC_FUNCTION_ENABLED
    // add a code for adding a public function "add_nums"
    // takes two arguments as input int x, and int y
    // return an int
    // (define-public (add_nums (num1 int) (num2 int)) (+ num1 num2))
    // + is a binary operator. it should be handled as such.
    // a binary operator in esbmc assumes an LHS and a RHS and an opcode.

    // steps :
    // create symbol add_nums
    std::string functionName = "add_nums";
    std::string functionId = "clar:@C@basicDummy@F@" + functionName + "#123";

    locationt locationBegin;
    locationBegin.set_line("293");
    locationBegin.set_file("basicDummy.clar");

    std::string debugModuleName = locationBegin.file().as_string();

    //function return type
    code_typet symbolType; // = signedbv_typet(128);
    symbolType.return_type() = signedbv_typet(128);

    symbolt symbol;
    get_default_symbol(
      symbol,
      debugModuleName,
      symbolType,
      functionName,
      functionId,
      locationBegin);

    symbol.lvalue = true;
    symbol.file_local = false;
    symbol.is_extern = false;

    symbolt &addedSymbol = *move_symbol_to_context(symbol);

    //till now :  function name has been added to context

    // get parameters list from AST
    // for each param, do the following
    // each param is a code_typet::argumentt type -> this is an exprt type

    //argument 1
    code_typet::argumentt param1;
    param1.type() = signedbv_typet(128);
    std::string param1_name = "num1";
    param1.name(param1_name);
    std::string param1_id =
      "clar:@C@basicDummy@F@" + functionName + "@" + param1_name + "#123";
    //param1.id( param1_id);
    param1.cmt_base_name(param1_name);

    locationt param1_location;
    param1_location.set_line("111");
    param1_location.set_file("basicDummy.clar");
    // we set cmt_identifier instead of type.id for arguments
    param1.cmt_identifier(param1_id);
    param1.location() = param1_location;

    std::string debug_modulename =
      get_modulename_from_path(param1_location.file().as_string());

    symbolt param1_symbol;
    get_default_symbol(
      param1_symbol,
      debug_modulename,
      param1.type(),
      param1_name,
      param1_id,
      param1_location);
    param1_symbol.lvalue = true;
    param1_symbol.is_parameter = true;
    param1_symbol.file_local = true;

    symbolt &added_param1_symbol = *move_symbol_to_context(param1_symbol);

    //argument 2
    code_typet::argumentt param2;

    param2.type() = signedbv_typet(128);
    std::string param2_name = "num2";
    param2.name(param2_name);
    std::string param2_id =
      "clar:@C@basicDummy@F@" + functionName + "@" + param2_name + "#111";
    param2.cmt_base_name(param2_name);

    locationt param2_location;
    param2_location.set_line("111");
    param2_location.set_file("basicDummy.clar");
    param2.cmt_identifier(param2_id);
    param2.location() = param2_location;

    symbolt param2_symbol;
    get_default_symbol(
      param2_symbol,
      debug_modulename,
      param2.type(),
      param2_name,
      param2_id,
      param2_location);
    param2_symbol.lvalue = true;
    param2_symbol.is_parameter = true;
    param2_symbol.file_local = true;

    symbolt &added_param2_symbol = *move_symbol_to_context(param2_symbol);

    // add parameters to function type

    symbolType.arguments().push_back(param1);
    symbolType.arguments().push_back(param2);

    addedSymbol.type = symbolType;

    // create function body
    // create a codet type exprt

    //code_blockt functionBody = code_blockt();
    // create a binary expression
    //plus_exprt binExpr(symbol_exprt(param1_symbol),symbol_exprt(param2_symbol),signedbv_typet(128));

    exprt param1_expr = symbol_expr(added_param1_symbol);
    exprt param2_expr = symbol_expr(added_param2_symbol);

    plus_exprt binExpr(param1_expr, param2_expr);

    code_returnt returnStatement;
    returnStatement.return_value() = binExpr;
    irept param1_irept;
    irept param2_irept;
    added_param1_symbol.to_irep(param1_irept);
    added_param2_symbol.to_irep(param2_irept);
    // std::cout <<"Param 1 symbol : " << param1_irept.pretty(4) <<"\n";
    // std::cout <<"Param 2 symbol : " << param2_irept.pretty(4) <<"\n";
    // std::cout <<"Param 1 expr: " << param1_expr.pretty(4) <<"\n";
    // std::cout <<"Param 2 expr: " << param2_expr.pretty(4) <<"\n";
    // std::cout <<"Bin Expr : " << binExpr.pretty(4) <<"\n";
    // std::cout <<"Return Statement : " <<returnStatement.pretty(4) <<"\n";
    // std::cout <<"Function Body : " <<functionBody.pretty(4) <<"\n";

    //functionBody.swap(returnStatement);

    // // refer to get_block function
    //   functionBody.operands().push_back(returnStmt);// .add(to_code_return(returnStmt));

    // add function body to the symbol
    addedSymbol.value = returnStatement;
#endif
  }
  else
  {
  }
}

bool clarity_convertert::check_valid_ast(const nlohmann::json &ast_node)
{
  if (
    !src_ast_json.contains(
      "identifier")) // check json file contains expression AST nodes as Clarity might change
    assert(!"JSON file does not contain any expression AST nodes");
  return true;
}
bool clarity_convertert::convert()
{
  // This function consists of two parts:
  //  1. First, we perform pattern-based verificaiton (ToDo)
  //  2. Then we populate the context with symbols annotated based on the each AST node, and hence prepare for the GOTO conversion.

  check_valid_ast(src_ast_json);

  absolute_path = current_fileName =
    contract_path; // this is set in the constructor

  // By now the context should have the symbols of all ESBMC's intrinsics and the dummy main
  // We need to convert Clarity AST nodes to the equivalent symbols and add them to the context
  nlohmann::json &master_node = src_ast_json;
  nlohmann::json &nodes = src_ast_json; //src_ast_json["expressions"];

  size_t index = 0;

  bool found_contract_def = false;

  // find contract_identifier to verify if the ast is valid

  if (master_node.contains("identifier"))
  {
    //nodes = master_node["identifier"];
    std::string contract_name = master_node["identifier"]["contract_name"];
    set_current_contract_name(contract_name);
    std::string issuer = master_node["identifier"]["issuer_principal"];

    //proper contract definition found
    found_contract_def = true;

    current_contract_issuer = issuer;

    // not sure what we'd use this for
    if (get_clarity_struct_class(master_node))
      return true;

    // add implicit construcor function
    if (add_implicit_constructor())
      return true;

    log_status(
      "Contract\t: {} \nIssuer\t: {}",
      current_contractName,
      current_contract_issuer);
  }
  else
  {
    assert(!"AST JSON does not have contract name");
  }

  assert(found_contract_def && "No contracts were found in the program.");
  // Pattern based checking e-g Solidity checks for SWC-115

  log_status(
    "Pattern based checking goes here . Solidity frontend had SWC-115 tested "
    "here.\nSkipping for Clarity for now. ");
#if 0
  FIXME : (m-ali) we need to add pattern checking logic here e-g solidity checks for SVC 115
  for (nlohmann::json::iterator itr = nodes.begin(); itr != nodes.end();
       ++itr, ++index)
  {
    ;

  


    // ignore the meta information and locate nodes in ContractDefinition
    std::string node_type = (*itr)["nodeType"].get<std::string>();
    if (node_type == "ContractDefinition") // contains AST nodes we need
    {
      global_scope_id = (*itr)["id"];
      found_contract_def = true;

      assert(itr->contains("nodes"));
      auto pattern_check =
        std::make_unique<pattern_checker>((*itr)["nodes"], clar_func);
      if (pattern_check->do_pattern_check())
        return true; // 'true' indicates something goes wrong.
    }

    
  }
#endif

  // Exported symbols need to be checked here
  //
  // FIXME :  (m-ali) @Faried bhai, does Clarity have exported Symbols.
  // we have exported symbols in "exported_functions" node in the AST
  log_status("What are exported symbols in Clarity ?\n ");

  log_status("Exported symbols :");
  if (!src_ast_json["exported_functions"].is_array())
    assert(!"Exported functions is not an array");

  for (const auto &itr : src_ast_json["exported_functions"])
  {
    //! Assume it has only one id
    std::string c_name = itr;
    log_status("{}", c_name);
    //exportedSymbolsList.insert(std::pair<int, std::string>(c_id, c_name));
  }

  log_status("---------------------------");

  // first round: handle definitions that can be outside of the contract
  // including struct, enum, interface, event, error, library...
  // noted that some can also be inside the contract, e.g. struct, enum...
  index = 0;

  log_status(
    "What are nonContract definitions , definitions outside of the contract in "
    "terms of clarity.\nSkipping for now.");
  for (nlohmann::json::iterator itr = nodes.begin(); itr != nodes.end();
       ++itr, ++index)
  {
#if 0
    
    if (get_noncontract_defition(*itr))
      return true;

#endif
  }

  // second round: populate linearizedBaseList
  // this is to obtain the contract name list
  // seems like this is something needed for inheritance in contracts.
  // does that apply to Clarity ? I don't think so.
  // just using current_contractName should work fine.

  //FIXME : (m-ali) Do we need this for clarity ?

  //this is a dummy linearizedBaseList
  linearizedBaseList[current_contractName].push_back(9999);
  assert(!linearizedBaseList[current_contractName].empty());

  // index = 0;
  // for (nlohmann::json::iterator itr = nodes.begin(); itr != nodes.end();
  //      ++itr, ++index)
  // {
  //   std::string node_type = (*itr)["nodeType"].get<std::string>();

  //   if (node_type == "ContractDefinition") // rule source-unit
  //   {
  //     current_contractName = (*itr)["name"].get<std::string>();

  //     // poplulate linearizedBaseList
  //     // this is esstinally the calling order of the constructor
  //     for (const auto &id : (*itr)["linearizedBaseContracts"].items())
  //       linearizedBaseList[current_contractName].push_back(
  //         id.value().get<int>());
  //     assert(!linearizedBaseList[current_contractName].empty());
  //   }
  // }

  // third round: handle contract definition
  // single contract verification: where the option "--contract" is set.
  // multiple contracts verification: essentially verify the whole file.
  index = 0;
  //define_principal_struct();
  define_optional_type(
    "int128_t"); //for some reason, ESBMC doesn't allow BitInt inside template, but it allows the same if i define it here
  define_optional_type("uint128_t");
  //define_optional_type("bool");

  for (nlohmann::json::iterator itr = src_ast_json.begin();
       itr != src_ast_json.end();
       ++itr, ++index)
  {
    log_status("Node type {}", itr->type_name());

    // we got to skip contract_identifier as we have already read that part.
    if (std::string(itr->type_name()) == "object")
    {
      std::cout << "Key " << itr.key() << "\n";

      if (itr.key() == "identifier")
      {
        log_status("Skipping {} as we have already processed it", itr.key());
        continue;
      }
      else
        //log_status("Object not contract_identifier\n{}", itr->flatten());
        std::cout << "Object not contract_identifier\n" << itr->flatten();
    }
    else if (std::string(itr->type_name()) == "array")
    {
      std::cout << "Key " << itr.key() << "\n";

      /* Expressions Array is an array of Arrays.
        Each array element is an array. Each nested array is a mixed element array 
      */

      if (itr.key() == "expressions")
      {
        auto vec_expressions = itr.value();
        // handle expressions here.
        log_status("Found expressions array ...");
        std::cout << "Number of expressions in contract "
                  << current_contractName << "  : " << vec_expressions.size()
                  << "\n";

        //process expression array
        for (auto &expr : vec_expressions)
        {
          std::string decl_decorator = ClarityGrammar::get_declaration_decorator(expr);
          nlohmann::json expression_node = ClarityGrammar::get_expression_node(expr);
          std::string identifier = ClarityGrammar::get_expression_identifier(expression_node);
          
          
          log_status("Parsing {} {} ", decl_decorator, identifier);

//add_dummy_symbol();
// ml- no need to do this with new AST
#if 0
          if (ClarityGrammar::parse_expression_element(expr))
          {
            log_error("Invalid expression element");
            continue;
          }
#endif
          // for each element in expressions array
          // check if valid expression array

          if (convert_ast_nodes(expr))
            return true;
        }
      }
      else if (itr.key() == "exported_functions")
      {
        std::cout << " Exported functions not handled yet"
                  << "\n";
        continue;
      }
    }
    else
    {
      std::cout << "Key " << itr.key() << "\n";
      log_status("Unsupported type ...");
      continue;
    }

    // reset
    //current_contractName = "";
    //current_functionName = "";
    current_functionDecl = nullptr;
    current_forStmt = nullptr;
    global_scope_id = 0;
  }

  // Do Verification
  // single contract
  if (!tgt_cnt.empty() && tgt_func.empty())
  {
    // perform multi-transaction verification
    // by adding symbols to the "clar_main()" entry function
    if (multi_transaction_verification(tgt_cnt))
      return true;
  }
  // multiple contract
  if (tgt_func.empty() && tgt_cnt.empty())
  {
    if (multi_contract_verification())
      return true;
  }

  return false; // 'false' indicates successful completion.
}

bool clarity_convertert::convert_ast_nodes(const nlohmann::json &contract_def)
{
  size_t index = 0;

  nlohmann::json ast_node = contract_def;
  nlohmann::json expression_node = ClarityGrammar::get_expression_node(ast_node);
  
  std::string identifier_name = ClarityGrammar::get_expression_identifier(expression_node);
  std::string identifier_type = ClarityGrammar::get_expression_type(expression_node);

  
  

  // get_objtype_type_name(
  //   ast_node
  //     [1]["objtype"]); //std::string(ast_node[1]["objtype"][0]; //.type_name());
  log_debug(
    "clarity",
    "@@ Converting node[{}]: name={}, nodeType={} ...",
    index,
    identifier_name.c_str(),
    identifier_type.c_str());
  exprt dummy_decl;
  if (get_decl(ast_node, dummy_decl))
    return true;

  // After converting all AST nodes, current_functionDecl should be restored to nullptr.
  assert(current_functionDecl == nullptr);

  return false;
}

bool clarity_convertert::get_decl(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  new_expr = code_skipt();

  nlohmann::json ast_expression_node = ClarityGrammar::get_expression_node(ast_node);
  
  ClarityGrammar::ContractBodyElementT type =
    ClarityGrammar::get_contract_body_element_t(ast_expression_node);

  // based on each element as in Solidty grammar "rule contract-body-element"
  switch (type)
  {
  case ClarityGrammar::ContractBodyElementT::VarDecl:
  {
    return get_var_decl(ast_node, new_expr); // rule state-variable-declaration
  }
  case ClarityGrammar::ContractBodyElementT::FunctionDef:
  {
    return get_function_definition(ast_node); // rule function-definition
  }
  default:
  {
    log_error("Unimplemented type in rule contract-body-element");
    return true;
  }
  }

  return false;
}

bool clarity_convertert::get_var_decl_stmt(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  // For rule variable-declaration-statement
  new_expr = code_skipt();

  if (!ast_node.contains("nodeType"))
    assert(!"Missing \'nodeType\' filed in ast_node");

  ClarityGrammar::VarDeclStmtT type =
    ClarityGrammar::get_var_decl_stmt_t(ast_node);
  log_debug(
    "clarity",
    "	@@@ got Variable-declaration-statement: "
    "ClarityGrammar::VarDeclStmtT::{}",
    ClarityGrammar::var_decl_statement_to_str(type));

  switch (type)
  {
  case ClarityGrammar::VarDeclStmtT::VariableDecl:
  {
    return get_var_decl(ast_node, new_expr); // rule variable-declaration
  }
  default:
  {
    assert(!"Unimplemented type in rule variable-declaration-statement");
    return true;
  }
  }

  return false;
}

// rule state-variable-declaration
// rule variable-declaration-statement
bool clarity_convertert::get_var_decl(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  // For Clarity rule state-variable-declaration:
  // 1. populate typet
  typet t;
  nlohmann::json ast_expression_node = ClarityGrammar::get_expression_node(ast_node);
  // VariableDeclaration node contains both "typeName" and "typeDescriptions".
  // However, ExpressionStatement node just contains "typeDescriptions".
  // For consistensy, we use ["typeName"]["typeDescriptions"] as in state-variable-declaration
  // to improve the re-usability of get_type* function, when dealing with non-array var decls.
  // For array, do NOT use ["typeName"]. Otherwise, it will cause problem
  // when populating typet in get_cast

  bool mapping = false; //is_child_mapping(ast_node);
  if (mapping)
  {
    // the mapping should not handled in var decl, instead
    // it should be an expression inside the function.

    // 1. get the expr
    if (get_expr(ast_node["typeName"], new_expr))
      return true;

    // 2. move it to a function.
    if (current_functionDecl)
    {
      // trace:
      //        get_function_definition =>
      //        get_block => get_statement =>
      //        get_var_decl_stmt => get_var_decl
      //
      // Beside, it should always have an initial value, otherwise:
      // "Uninitialized mapping. Mappings cannot be created dynamically, you have to assign them from a state variable."
      // Do nothing since we have already updated the new_expr (was "code_skipt").
      return false;
    }
    else
    {
      // assume it's not inside a funciton, then move it to the ctor
      std::string contract_name;
      if (get_current_contract_name(ast_node, contract_name))
        return true;
      if (contract_name.empty())
        return true;
      // add an implict ctor if it's not declared explictly
      if (add_implicit_constructor())
        return true;
      symbolt &ctor =
        *context.find_symbol("clar:@" + contract_name + "@F@" + contract_name);
      ctor.value.operands().push_back(new_expr);

      return false;
    }
  }
  else
  {
    // requires type name as first param. but we don't have type inferred here. We got to add that to the node info

    nlohmann::json expression_objtype = ClarityGrammar::get_expression_objtype(ast_expression_node);
    
    if (get_type_description(expression_objtype, t))
      return true;
  }

  //bool is_state_var = ast_node["stateVariable"] == true;
  bool is_state_var = ClarityGrammar::is_state_variable(ast_node[0]);

  // 2. populate id and name
  std::string name, id;

  if (is_state_var)
    get_state_var_decl_name(ast_expression_node, name, id);
  else if (current_functionDecl)
  {
    assert(current_functionName != "");
    get_var_decl_name(ast_expression_node, name, id);
  }
  else
  {
    log_error("ESBMC could not find the parent scope for this local variable");
    return true;
  }

  // 3. populate location
  locationt location_begin;
  get_location_from_decl(ast_expression_node, location_begin);

  // 4. populate debug module name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());

  // 5. set symbol attributes
  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, t, name, id, location_begin);

  symbol.lvalue = true;
  symbol.static_lifetime = is_state_var;
  symbol.file_local = !is_state_var;
  symbol.is_extern = false;

  // initialise with zeroes if no initial value provided.
  bool has_init = ast_expression_node.contains(
    "value"); // in clarity we do not use "initialValue"
  if (symbol.static_lifetime && !symbol.is_extern && !has_init)
  {
    // set default value as zero
    symbol.value = gen_zero(t, true);
    symbol.value.zero_initializer(true);
  }

  // FIXME : add exception for if we don't have value node

  // 6. add symbol into the context
  // just like clang-c-frontend, we have to add the symbol before converting the initial assignment
  symbolt &added_symbol = *move_symbol_to_context(symbol);

  // 7. populate init value if there is any
  code_declt decl(symbol_expr(added_symbol));

  if (has_init)
  {
    nlohmann::json init_value = ClarityGrammar::get_expression_value_node(ast_expression_node);
    nlohmann::json objtype = ClarityGrammar::get_expression_objtype(ast_expression_node);
    
    

    //this might cause issue
    nlohmann::json literal_type = get_objtype_type_name(objtype);

    assert(literal_type != nullptr);
    exprt val;
    // we will pass the whole ast node everywhere.
    // we can then parse inside the sub-functions accordingly.
    // for initial value : consider looking into ast_node[1]["value"]
    if (get_expr(init_value, objtype, val))
      return true;

    clarity_gen_typecast(ns, val, t);

    added_symbol.value = val;
    decl.operands().push_back(val);
  }

  // special handle for contract type
  // e.g.
  //  Base x ==> Base x = new Base();
  else if (
    ClarityGrammar::get_type_name_t(ast_node["typeName"]["typeDescriptions"]) ==
    ClarityGrammar::ContractTypeName)
  {
    // 1. get constract name
    assert(
      ast_node["typeName"]["nodeType"].get<std::string>() ==
      "UserDefinedTypeName");
    const std::string contract_name =
      ast_node["typeName"]["pathNode"]["name"].get<std::string>();

    // 2. since the contract type variable has no initial value, i.e. explicit constructor call,
    // we construct an implicit constructor expression
    exprt val;
    if (get_implicit_ctor_ref(val, contract_name))
      return true;

    // 3. make it to a temporary object
    side_effect_exprt tmp_obj("temporary_object", val.type());
    codet code_expr("expression");
    code_expr.operands().push_back(val);
    tmp_obj.initializer(code_expr);
    tmp_obj.location() = val.location();
    val.swap(tmp_obj);

    // 4. generate typecast for Clarity contract
    clarity_gen_typecast(ns, val, t);

    // 5. add constructor call to declaration operands
    added_symbol.value = val;
    decl.operands().push_back(val);
  }

  decl.location() = location_begin;
  new_expr = decl;

  return false;
}

// This function handles both contract and struct
// The contract can be regarded as the class in C++, converting to a struct
bool clarity_convertert::get_clarity_struct_class(
  const nlohmann::json &struct_def)
{
  // 1. populate name, id
  std::string id, name;
  struct_typet t = struct_typet();

  get_current_contract_name(struct_def, name);
  id = prefix + name;
  t.tag(name);

  // 2. Check if the symbol is already added to the context, do nothing if it is
  // already in the context.
  if (context.find_symbol(id) != nullptr)
    return false;

  // 3. populate location
  locationt location_begin;
  location_begin.set_line(0);
  location_begin.set_file(absolute_path);

  // 4. populate debug module name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());
  current_fileName = debug_modulename;

  symbolt symbol;
  log_status("get_clarity_struct_class adding symbols {} ", id);
  get_default_symbol(symbol, debug_modulename, t, name, id, location_begin);

  symbol.is_type = true;
  symbolt &added_symbol = *move_symbol_to_context(symbol);

  // populate the scope_map
  // this map is used to find reference when there is no decl_ref_id provided in the nodes
  // or replace the find_decl_ref in order to speed up
  // using 9999 is the top most scope.
  int scp = 9999; //struct_def["id"].get<int>();
  scope_map.insert(std::pair<int, std::string>(scp, name));

  // 5. populate fields(state var) and method(function)
  // We have to add fields before methods as the fields are likely to be used
  // in the methods

  //m-ali todo . we probably don't need this.

  // nlohmann::json ast_nodes;
  // if (struct_def.contains("nodes"))
  //   ast_nodes = struct_def["nodes"];
  // else if (struct_def.contains("members"))
  //   ast_nodes = struct_def["members"];
  // else
  // {
  //   // Defining empty structs is disallowed.
  //   // Contracts can be empty
  //   log_warning("Empty contract.");
  // }

  // m-ali : todo . we probably don't need this

  // for (nlohmann::json::iterator itr = ast_nodes.begin(); itr != ast_nodes.end();
  //      ++itr)
  // {
  //   ClarityGrammar::ContractBodyElementT type =
  //     ClarityGrammar::get_contract_body_element_t(*itr);

  //   switch (type)
  //   {
  //   case ClarityGrammar::ContractBodyElementT::VarDecl:
  //   {
  //     // this can be both state and non-state variable
  //     if (get_struct_class_fields(*itr, t))
  //       return true;
  //     break;
  //   }
  //   case ClarityGrammar::ContractBodyElementT::FunctionDef:
  //   {
  //     if (get_struct_class_method(*itr, t))
  //       return true;
  //     break;
  //   }
  //   default:
  //   {
  //     log_error("Unimplemented type in rule contract-body-element");
  //     return true;
  //   }
  //   }
  // }

  t.location() = location_begin;
  added_symbol.type = t;

  return false;
}

// This function handles both contract and struct
// The contract can be regarded as the class in C++, converting to a struct
bool clarity_convertert::get_struct_class(const nlohmann::json &struct_def)
{
  // 1. populate name, id
  std::string id, name;
  struct_typet t = struct_typet();

  name = struct_def["name"].get<std::string>();
  id = prefix + name;
  t.tag(name);

  // 2. Check if the symbol is already added to the context, do nothing if it is
  // already in the context.
  if (context.find_symbol(id) != nullptr)
    return false;

  // 3. populate location
  locationt location_begin;
  get_location_from_decl(struct_def, location_begin);

  // 4. populate debug module name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());
  current_fileName = debug_modulename;

  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, t, name, id, location_begin);

  symbol.is_type = true;
  symbolt &added_symbol = *move_symbol_to_context(symbol);

  // populate the scope_map
  // this map is used to find reference when there is no decl_ref_id provided in the nodes
  // or replace the find_decl_ref in order to speed up
  int scp = 9999; //struct_def["id"].get<int>();
  scope_map.insert(std::pair<int, std::string>(scp, name));

  // 5. populate fields(state var) and method(function)
  // We have to add fields before methods as the fields are likely to be used
  // in the methods

  nlohmann::json ast_nodes;
  if (struct_def.contains("nodes"))
    ast_nodes = struct_def["nodes"];
  else if (struct_def.contains("members"))
    ast_nodes = struct_def["members"];
  else
  {
    // Defining empty structs is disallowed.
    // Contracts can be empty
    log_warning("Empty contract.");
  }

  for (nlohmann::json::iterator itr = ast_nodes.begin(); itr != ast_nodes.end();
       ++itr)
  {
    ClarityGrammar::ContractBodyElementT type =
      ClarityGrammar::get_contract_body_element_t(*itr);

    switch (type)
    {
    case ClarityGrammar::ContractBodyElementT::VarDecl:
    {
      // this can be both state and non-state variable
      if (get_struct_class_fields(*itr, t))
        return true;
      break;
    }
    case ClarityGrammar::ContractBodyElementT::FunctionDef:
    {
      if (get_struct_class_method(*itr, t))
        return true;
      break;
    }
    default:
    {
      log_error("Unimplemented type in rule contract-body-element");
      return true;
    }
    }
  }

  t.location() = location_begin;
  added_symbol.type = t;

  return false;
}

bool clarity_convertert::get_struct_class_fields(
  const nlohmann::json &ast_node,
  struct_typet &type)
{
  struct_typet::componentt comp;

  if (get_var_decl_ref(ast_node, comp))
    return true;

  comp.id("component");
  comp.type().set("#member_name", type.tag());

  if (get_access_from_decl(ast_node, comp))
    return true;
  type.components().push_back(comp);

  return false;
}

bool clarity_convertert::get_struct_class_method(
  const nlohmann::json &ast_node,
  struct_typet &type)
{
  struct_typet::componentt comp;
  if (get_func_decl_ref(ast_node, comp))
    return true;

  if (comp.is_code() && to_code(comp).statement() == "skip")
    return false;

  if (get_access_from_decl(ast_node, comp))
    return true;

  type.methods().push_back(comp);
  return false;
}

bool clarity_convertert::get_noncontract_defition(nlohmann::json &ast_node)
{
  std::string node_type = (ast_node)["nodeType"].get<std::string>();

  if (node_type == "StructDefinition")
  {
    if (get_struct_class(ast_node))
      return true;
  }
  else if (node_type == "EnumDefinition")
    // set the ["Value"] for each member inside enum
    add_enum_member_val(ast_node);
  else if (node_type == "ErrorDefinition")
  {
    if (get_error_definition(ast_node))
      return true;
  }

  return false;
}

void clarity_convertert::add_enum_member_val(nlohmann::json &ast_node)
{
  /*
  "nodeType": "EnumDefinition",
  "members":
    [
      {
          "id": 2,
          "name": "SMALL",
          "nameLocation": "66:5:0",
          "nodeType": "EnumValue",
          "src": "66:5:0",
          "Value": 0 => new added object
      },
      {
          "id": 3,
          "name": "MEDIUM",
          "nameLocation": "73:6:0",
          "nodeType": "EnumValue",
          "src": "73:6:0",
          "Value": 1  => new added object
      },
    ] */

  assert(ast_node["nodeType"] == "EnumDefinition");
  int idx = 0;
  nlohmann::json &members = ast_node["members"];
  for (nlohmann::json::iterator itr = members.begin(); itr != members.end();
       ++itr, ++idx)
  {
    (*itr).push_back(
      nlohmann::json::object_t::value_type("Value", std::to_string(idx)));
  }
}

// covert the error_definition to a function
bool clarity_convertert::get_error_definition(const nlohmann::json &ast_node)
{
  // e.g.
  // error errmsg(int num1, uint num2, uint[2] addrs);
  //   to
  // function 'tag-erro errmsg@12'() { __ESBMC_assume(false);}

  const nlohmann::json *old_functionDecl = current_functionDecl;
  const std::string old_functionName = current_functionName;

  // e.g. name: errmsg; id: clar:@errmsg#12
  const int id_num = ast_node["id"].get<int>();
  std::string name, id;
  name = ast_node["name"].get<std::string>();
  id = "clar:@" + name + "#" + std::to_string(id_num);

  // update scope map
  scope_map.insert(std::pair<int, std::string>(id_num, name));

  // just to pass the internal assertions
  current_functionName = name;
  current_functionDecl = &ast_node;

  // no return value
  code_typet type;
  type.return_type() = empty_typet();

  locationt location_begin;
  get_location_from_decl(ast_node[0], location_begin);
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());

  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, type, name, id, location_begin);
  symbol.lvalue = true;

  symbolt &added_symbol = *move_symbol_to_context(symbol);

  // populate the params
  ClarityGrammar::ParameterListT params =
    ClarityGrammar::get_parameter_list_t(ast_node["parameters"]);
  if (params == ClarityGrammar::ParameterListT::EMPTY)
    type.make_ellipsis();
  else
  {
    for (const auto &decl : ast_node["parameters"]["parameters"].items())
    {
      const nlohmann::json &func_param_decl = decl.value();

      code_typet::argumentt param;
      if (get_function_params(func_param_decl, param))
        return true;

      type.arguments().push_back(param);
    }
  }
  added_symbol.type = type;

  // construct a "__ESBMC_assume(false)" statement
  typet return_type = bool_type();
  return_type.set("#cpp_type", "bool");
  code_typet convert_type;
  convert_type.return_type() = return_type;

  exprt statement = exprt("symbol", convert_type);
  statement.cmt_lvalue(true);
  statement.name("__ESBMC_assume");
  statement.identifier("__ESBMC_assume");

  side_effect_expr_function_callt call;
  call.function() = statement;
  call.type() = return_type;
  exprt arg = false_exprt();
  call.arguments().push_back(arg);
  convert_expression_to_code(call);

  // insert it to the body
  code_blockt body;
  body.operands().push_back(call);
  added_symbol.value = body;

  // restore
  current_functionDecl = old_functionDecl;
  current_functionName = old_functionName;

  return false;
}

bool clarity_convertert::add_implicit_constructor()
{
  std::string name, id;
  name = current_contractName;

  id = get_ctor_call_id(current_contractName);
  if (context.find_symbol(id) != nullptr)
    return false;

  // an implicit constructor is an void empty function
  return get_default_function(name, id);
}

bool clarity_convertert::get_access_from_decl(
  const nlohmann::json &ast_node,
  struct_typet::componentt &comp)
{
  std::string access = ast_node["visibility"].get<std::string>();
  comp.set_access(access);

  return false;
}

bool clarity_convertert::get_function_definition(const nlohmann::json &ast_node)
{
  // For Clarity rule function-definition:
  // Order matters! do not change!
  // 1. Check fd.isImplicit() --- skipped since it's not applicable to Clarity
  // 2. Check fd.isDefined() and fd.isThisDeclarationADefinition()
  
#if 0 
  // ml- need to check if there is such a thing as intrinsic function
  if (
    !ast_node
      ["implemented"]) // TODO: for interface function, it's just a definition. Add something like "&& isInterface_JustDefinition()"
    return false;

  // Check intrinsic functions
  if (check_intrinsic_function(ast_node))
    return false;
#endif
  // 3. Set current_scope_var_num, current_functionDecl and old_functionDecl
  current_scope_var_num = 1;
  const nlohmann::json *old_functionDecl = current_functionDecl;
  const std::string old_functionName = current_functionName;

  current_functionDecl = &ast_node;
  bool is_ctor = false;
#if 0 
  // no option of a constructor type
  if (
    (*current_functionDecl)["name"].get<std::string>() == "" &&
    (*current_functionDecl)["kind"] == "constructor")
  {
    is_ctor = true;
    if (get_current_contract_name(*current_functionDecl, current_functionName))
      return true;
  }
  else
#endif
  
  current_functionName =
    (*current_functionDecl)[1]["identifier"].get<std::string>();
  log_debug(
    "clarity",
    "	@@@ get_function_definition processing function {}",
    current_functionName);

  // 4. Return type
  code_typet type;

  if ((get_type_description(ast_node[1]["return_type"], type.return_type())))
    return true;
  
// special handling for return_type:
#if 0
  // [TODO] will deel with return types later as these are complex
  // construct a tuple type and a tuple instance
  if (type.return_type().get("#clar_type") == "return_type")
  {
    exprt dump;
    if (get_tuple_definition(*current_functionDecl))
      return true;
    if (get_tuple_instance(*current_functionDecl, dump))
      return true;
    type.return_type().set("#clar_tuple_id", dump.identifier().as_string());
  }
#endif

  // 5. Check fd.isVariadic(), fd.isInlined()
  //  Skipped since Clarity does not support variadic (optional args) or inline function.
  //  Actually "inline" doesn not make sense in Clarity

  // 6. Populate "locationt location_begin"
  locationt location_begin;
  get_location_from_decl(ast_node[1], location_begin);

  // 7. Populate "std::string id, name"
  std::string name, id;
  get_function_definition_name(ast_node[1], name, id);

  if (name == "func_dynamic")
    printf("@@ found func_dynamic\n");

  // 8. populate "std::string debug_modulename"
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());

  // 9. Populate "symbol.static_lifetime", "symbol.is_extern" and "symbol.file_local"
  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, type, name, id, location_begin);

  symbol.lvalue = true;
  symbol.is_extern =
    false; // TODO: hard coded for now, may need to change later
  symbol.file_local = false;

  // 10. Add symbol into the context
  symbolt &added_symbol = *move_symbol_to_context(symbol);

// 11. Convert parameters, if no parameter, assume ellipis
//  - Convert params before body as they may get referred by the statement in the body
#if 0
  // ml- No need of constructors
  if (is_ctor)
  {
    /* need (type *) as first parameter, this is equivalent to the 'this'
     * pointer in C++ */
    code_typet::argumentt param(pointer_typet(type.return_type()));
    type.arguments().push_back(param);
  }
#endif

  ClarityGrammar::ParameterListT params =
    ClarityGrammar::get_parameter_list_t(ast_node[1]);

  if (params != ClarityGrammar::ParameterListT::EMPTY)
  {
    // convert parameters if the function has them
    // update the typet, since typet contains parameter annotations
    for (const auto &decl : ast_node[1]["args"].items())
    {
      const nlohmann::json &func_param_decl = decl.value();

      code_typet::argumentt param;
      if (get_function_params(func_param_decl, param))
        return true;

      type.arguments().push_back(param);
    }
  }

  NewFunction(type);

  added_symbol.type = type;

  // 12. Convert body and embed the body into the same symbol
  if (ast_node[1].contains("body"))
  {
    exprt body_exprt;
    if (get_function_body(
          ast_node[1]["body"], body_exprt, type, ast_node[1]["return_type"]))
      return true;

    added_symbol.value = body_exprt;
  }

  //assert(!"done - finished all expr stmt in function?");

  // 13. Restore current_functionDecl
  current_functionDecl =
    old_functionDecl; // for __ESBMC_assume, old_functionDecl == null
  current_functionName = old_functionName;

  return false;
}

void clarity_convertert::NewFunction(code_typet &type)
{
  if (type.arguments().empty())
  {
    // assume ellipsis if the function has no parameters
    type.make_ellipsis();
  }
}

bool clarity_convertert::get_function_params(
  const nlohmann::json &pd,
  exprt &param)
{
  // 1. get parameter type
  typet param_type;
  if (get_type_description(pd["objtype"], param_type))
    return true;

  // 2a. get id and name
  std::string id, name;
  assert(current_functionName != ""); // we are converting a function param now
  assert(current_functionDecl);
  get_var_decl_name(pd, name, id);

  // 2b. handle Omitted Names in Function Definitions
  if (name == "")
  {
    // Items with omitted names will still be present on the stack, but they are inaccessible by name.
    // e.g. ~omitted1, ~omitted2. which is a invalid name for clarity.
    // Therefore it won't conflict with other arg names.
    //log_error("Omitted params are not supported");
    // return true;
    ;
  }

  param = code_typet::argumentt();
  param.type() = param_type;
  param.cmt_base_name(name);

  // 3. get location
  locationt location_begin;
  get_location_from_decl(pd, location_begin);

  param.cmt_identifier(id);
  param.location() = location_begin;

  // 4. get symbol
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());
  symbolt param_symbol;
  get_default_symbol(
    param_symbol, debug_modulename, param_type, name, id, location_begin);

  // 5. set symbol's lvalue, is_parameter and file local
  param_symbol.lvalue = true;
  param_symbol.is_parameter = true;
  param_symbol.file_local = true;

  // 6. add symbol to the context
  move_symbol_to_context(param_symbol);

  return false;
}

// input    : the body field of the functions AST
// output   : The new exprt created to represent the body
// output   : The new typet created to represent the function return
// input    : The objtype from the functions AST
// returns  : false if succesful, or true if failed.
bool clarity_convertert::get_function_body(
  const nlohmann::json &body,
  exprt &new_expr,
  typet &return_type,
  const nlohmann::json &return_objtype)
{
  // For function body
  locationt location;
  get_start_location_from_stmt(body, location);

  ClarityGrammar::FuncBodyT type = ClarityGrammar::get_function_body_t(body);
  log_debug(
    "clarity",
    "	@@@ got Function Body type: ClarityGrammar::FuncBodyT::{}",
    ClarityGrammar::function_body_to_str(type));

  switch (type)
  {
    // clarity functions have a single body element mostly 
    case ClarityGrammar::FuncBodyT::SingleStatement:
    {
      // The function body in this case will be a single 
      // expression that has to be returned
      // ml-[TODO] we have to make a function that will create
      // any expression as return
      
      code_returnt ret_expr;
      const nlohmann::json &rtn_expr = body[0];

      // ml- the following code has been adapted from the 
      // ReturnStatement of solidity
      exprt val;
      if (get_expr(rtn_expr, return_objtype, val))
        return true;

      clarity_gen_typecast(ns, val, return_type);
      ret_expr.return_value() = val;

      new_expr = ret_expr;
      break;
    }
    case ClarityGrammar::FuncBodyT::MultipleStatement:
    {
      // ml - [TODO] Handle case where a body has multiple statements
      //get_expr(block["expression"], new_expr);
      break;
    }
    default:
    {
      assert(!"Unimplemented type in rule  block");
      return true;
    }
  }

  new_expr.location() = location;
  return false;
}

bool clarity_convertert::get_block(const nlohmann::json &block, exprt &new_expr)
{
  log_status(
    "clarity"
    "	@@@ got Block: ClarityGrammar::BlockT::{}",
    block.dump());

  // For rule block
  locationt location;
  get_start_location_from_stmt(block, location);

  ClarityGrammar::BlockT type = ClarityGrammar::get_block_t(block);
  log_status(
    "clarity",
    "	@@@ got Block: ClarityGrammar::BlockT::{}",
    ClarityGrammar::block_to_str(type));

  switch (type)
  {
  // equivalent to clang::Stmt::CompoundStmtClass
  // deal with a block of statements
  case ClarityGrammar::BlockT::Statement:
  {
    const nlohmann::json &stmts = block["statements"];

    code_blockt _block;
    unsigned ctr = 0;
    // items() returns a key-value pair with key being the index
    for (auto const &stmt_kv : stmts.items())
    {
      exprt statement;
      if (get_statement(stmt_kv.value(), statement))
        return true;

      convert_expression_to_code(statement);
      _block.operands().push_back(statement);
      ++ctr;
    }
    log_debug("clarity", " \t@@@ CompoundStmt has {} statements", ctr);

    locationt location_end;
    get_final_location_from_stmt(block, location_end);

    _block.end_location(location_end);
    new_expr = _block;
    break;
  }
  case ClarityGrammar::BlockT::BlockForStatement:
  case ClarityGrammar::BlockT::BlockIfStatement:
  {
    // this means only one statement in the block
    exprt statement;

    // pass directly to get_statement()
    if (get_statement(block, statement))
      return true;
    convert_expression_to_code(statement);
    new_expr = statement;
    break;
  }
  case ClarityGrammar::BlockT::BlockExpressionStatement:
  {
    get_expr(block["expression"], new_expr);
    break;
  }
  default:
  {
    assert(!"Unimplemented type in rule block");
    return true;
  }
  }

  new_expr.location() = location;
  return false;
}

bool clarity_convertert::get_statement(
  const nlohmann::json &stmt,
  exprt &new_expr)
{
  // For rule statement
  // Since this is an additional layer of grammar rules compared to clang C, we do NOT set location here.
  // Just pass the new_expr reference to the next layer.

  ClarityGrammar::StatementT type = ClarityGrammar::get_statement_t(stmt);
  log_debug(
    "clarity",
    "	@@@ got Stmt: ClarityGrammar::StatementT::{}",
    ClarityGrammar::statement_to_str(type));

  switch (type)
  {
  case ClarityGrammar::StatementT::Block:
  {
    if (get_block(stmt, new_expr))
      return true;
    break;
  }
  case ClarityGrammar::StatementT::ExpressionStatement:
  {
    if (get_expr(stmt["expression"], new_expr))
      return true;
    break;
  }
  case ClarityGrammar::StatementT::VariableDeclStatement:
  {
    const nlohmann::json &declgroup = stmt["declarations"];

    codet decls("decl-block");
    unsigned ctr = 0;
    // N.B. Although Clarity AST JSON uses "declarations": [],
    // the size of this array is alway 1!
    // A second declaration will become another stmt in "statements" array
    // e.g. "statements" : [
    //  {"declarations": [], "id": 1}
    //  {"declarations": [], "id": 2}
    //  {"declarations": [], "id": 3}
    // ]
    for (const auto &it : declgroup.items())
    {
      // deal with local var decl with init value
      nlohmann::json decl = it.value();
      if (stmt.contains("initialValue"))
      {
        // Need to combine init value with the decl JSON object
        decl["initialValue"] = stmt["initialValue"];
      }

      exprt single_decl;
      if (get_var_decl_stmt(decl, single_decl))
        return true;

      decls.operands().push_back(single_decl);
      ++ctr;
    }
    log_debug("clarity", " \t@@@ DeclStmt group has {} decls", ctr);

    new_expr = decls;
    break;
  }
  case ClarityGrammar::StatementT::ReturnStatement:
  {
    if (!current_functionDecl)
    {
      log_error(
        "Error: ESBMC could not find the parent scope for this "
        "ReturnStatement");
      return true;
    }

    // 1. get return type
    // TODO: Fix me! Assumptions:
    //  a). It's "return <expr>;" not "return;"
    //  b). <expr> is pointing to a DeclRefExpr, we need to wrap it in an ImplicitCastExpr as a subexpr
    //  c). For multiple return type, the return statement represented as a tuple expression using a components field.
    //      Besides, tuple can only be declared literally. https://docs.claritylang.org/en/latest/control-structures.html#assignment
    //      e.g. return (false, 123)
    assert(stmt.contains("expression"));
    assert(stmt["expression"].contains("nodeType"));

    // get_type_description
    typet return_exrp_type;
    if (get_type_description(
          stmt["expression"]["typeDescriptions"], return_exrp_type))
      return true;

    if (return_exrp_type.get("#clar_type") == "tuple")
    {
      if (
        stmt["expression"]["nodeType"].get<std::string>() !=
          "TupleExpression" &&
        stmt["expression"]["nodeType"].get<std::string>() != "FunctionCall")
      {
        log_error("Unexpected tuple");
        return true;
      }

      code_blockt _block;

      // get tuple instance
      std::string tname, tid;
      if (get_tuple_instance_name(*current_functionDecl, tname, tid))
        return true;
      if (context.find_symbol(tid) == nullptr)
        return true;

      // get lhs
      exprt lhs = symbol_expr(*context.find_symbol(tid));

      if (
        stmt["expression"]["nodeType"].get<std::string>() == "TupleExpression")
      {
        // return (x,y) ==>
        // tuple.mem0 = x; tuple.mem1 = y; return ;

        // get rhs
        exprt rhs;
        if (get_expr(stmt["expression"], rhs))
          return true;

        size_t ls = to_struct_type(lhs.type()).components().size();
        size_t rs = rhs.operands().size();
        assert(ls == rs);

        for (size_t i = 0; i < ls; i++)
        {
          // lop: struct member call (e.g. tuple.men0)
          exprt lop;
          if (get_tuple_member_call(
                lhs.identifier(),
                to_struct_type(lhs.type()).components().at(i),
                lop))
            return true;

          // rop: constant/symbol
          exprt rop = rhs.operands().at(i);

          // do assignment
          get_tuple_assignment(_block, lop, rop);
        }
      }
      else
      {
        // return func(); ==>
        // tuple1.mem0 = tuple0.mem0; return;

        // get rhs
        exprt rhs;
        if (get_tuple_function_ref(stmt["expression"]["expression"], rhs))
          return true;

        // add function call
        exprt func_call;
        if (get_expr(
              stmt["expression"],
              stmt["expression"]["typeDescriptions"],
              func_call))
          return true;
        get_tuple_function_call(_block, func_call);

        size_t ls = to_struct_type(lhs.type()).components().size();
        size_t rs = to_struct_type(rhs.type()).components().size();
        assert(ls == rs);

        for (size_t i = 0; i < ls; i++)
        {
          // lop: struct member call (e.g. tupleA.men0)
          exprt lop;
          if (get_tuple_member_call(
                lhs.identifier(),
                to_struct_type(lhs.type()).components().at(i),
                lop))
            return true;

          // rop: struct member call (e.g. tupleB.men0)
          exprt rop;
          if (get_tuple_member_call(
                rhs.identifier(),
                to_struct_type(rhs.type()).components().at(i),
                rop))
            return true;

          // do assignment
          get_tuple_assignment(_block, lop, rop);
        }
      }
      // do return in the end
      exprt return_expr = code_returnt();
      _block.move_to_operands(return_expr);

      if (_block.operands().size() == 0)
        new_expr = code_skipt();
      else
        new_expr = _block;
      break;
    }

    typet return_type;
    if ((*current_functionDecl).contains("returnParameters"))
    {
      assert(
        (*current_functionDecl)["returnParameters"]["id"]
          .get<std::uint16_t>() ==
        stmt["functionReturnParameters"].get<std::uint16_t>());
      if (get_type_description(
            (*current_functionDecl)["returnParameters"], return_type))
        return true;
    }
    else
      return true;

    nlohmann::json literal_type = nullptr;

    auto expr_type = ClarityGrammar::get_expression_t(stmt["expression"]);
    bool expr_is_literal = expr_type == ClarityGrammar::Literal;
    if (expr_is_literal)
      literal_type = make_return_type_from_typet(return_type);

    // 2. get return value
    code_returnt ret_expr;
    const nlohmann::json &rtn_expr = stmt["expression"];
    // wrap it in an ImplicitCastExpr to convert LValue to RValue
    nlohmann::json implicit_cast_expr =
      make_implicit_cast_expr(rtn_expr, "LValueToRValue");

    /* There could be case like
      {
      "expression": {
          "kind": "number",
          "nodeType": "Literal",
          "typeDescriptions": {
              "typeIdentifier": "t_rational_11_by_1",
              "typeString": "int_const 11"
          },
          "value": "12345"
      },
      "nodeType": "Return",
      }
      Therefore, we need to pass the literal_type value.
      */

    exprt val;
    if (get_expr(implicit_cast_expr, literal_type, val))
      return true;

    clarity_gen_typecast(ns, val, return_type);
    ret_expr.return_value() = val;

    new_expr = ret_expr;

    break;
  }
  case ClarityGrammar::StatementT::ForStatement:
  {
    // Based on rule for-statement

    // For nested loop
    const nlohmann::json *old_forStmt = current_forStmt;
    current_forStmt = &stmt;

    // 1. annotate init
    codet init =
      code_skipt(); // code_skipt() means no init in for-stmt, e.g. for (; i< 10; ++i)
    if (stmt.contains("initializationExpression"))
      if (get_statement(stmt["initializationExpression"], init))
        return true;

    convert_expression_to_code(init);

    // 2. annotate condition
    exprt cond = true_exprt();
    if (stmt.contains("condition"))
      if (get_expr(stmt["condition"], cond))
        return true;

    // 3. annotate increment
    codet inc = code_skipt();
    if (stmt.contains("loopExpression"))
      if (get_statement(stmt["loopExpression"], inc))
        return true;

    convert_expression_to_code(inc);

    // 4. annotate body
    codet body = code_skipt();
    if (stmt.contains("body"))
      if (get_statement(stmt["body"], body))
        return true;

    convert_expression_to_code(body);

    code_fort code_for;
    code_for.init() = init;
    code_for.cond() = cond;
    code_for.iter() = inc;
    code_for.body() = body;

    new_expr = code_for;
    current_forStmt = old_forStmt;
    break;
  }
  case ClarityGrammar::StatementT::IfStatement:
  {
    // Based on rule if-statement
    // 1. Condition: make a exprt for condition
    exprt cond;
    if (get_expr(stmt["condition"], cond))
      return true;

    // 2. Then: make a exprt for trueBody
    exprt then;
    if (get_statement(stmt["trueBody"], then))
      return true;

    convert_expression_to_code(then);

    codet if_expr("ifthenelse");
    if_expr.copy_to_operands(cond, then);

    // 3. Else: make a exprt for "falseBody" if the if-statement node contains an "else" block
    if (stmt.contains("falseBody"))
    {
      exprt else_expr;
      if (get_statement(stmt["falseBody"], else_expr))
        return true;

      convert_expression_to_code(else_expr);
      if_expr.copy_to_operands(else_expr);
    }

    new_expr = if_expr;
    break;
  }
  case ClarityGrammar::StatementT::ContinueStatement:
  {
    new_expr = code_continuet();
    break;
  }
  case ClarityGrammar::StatementT::BreakStatement:
  {
    new_expr = code_breakt();
    break;
  }
  case ClarityGrammar::StatementT::RevertStatement:
  {
    // e.g.
    // {
    //   "errorCall": {
    //     "nodeType": "FunctionCall",
    //   }
    //   "nodeType": "RevertStatement",
    // }
    if (!stmt.contains("errorCall") || get_expr(stmt["errorCall"], new_expr))
      return true;

    break;
  }
  case ClarityGrammar::StatementT::StatementTError:
  default:
  {
    log_error(
      "Unimplemented Statement type in rule statement. Got {}",
      ClarityGrammar::statement_to_str(type));
    return true;
  }
  }

  return false;
}

/**
     * @brief Populate the out parameter with the expression based on
     * the clarity expression grammar
     *
     * @param expr The expression ast is to be converted to the IR
     * @param new_expr Out parameter to hold the conversion
     * @return true iff the conversion has failed
     * @return false iff the conversion was successful
     */
bool clarity_convertert::get_expr(const nlohmann::json &expr, exprt &new_expr)
{
  return get_expr(expr, nullptr, new_expr);
}

bool clarity_convertert::get_expr(
  const nlohmann::json &expr,
  const nlohmann::json &literal_type,
  exprt &new_expr)
{
  nlohmann::json inferred_type;
  return get_expr(expr, nullptr, new_expr, inferred_type);
}
/**
     * @brief Populate the out parameter with the expression based on
     * the clarity expression grammar.
     *
     * More specifically, parse each expression in the AST json and
     * convert it to a exprt ("new_expr"). The expression may have sub-expression
     *
     * !Always check if the expression is a Literal before calling get_expr
     * !Unless you are 100% sure it will not be a constant
     *
     * This function is called throught two paths:
     * 1. get_decl => get_var_decl => get_expr
     * 2. get_decl => get_function_definition => get_statement => get_expr
     *
     * @param expr The expression that is to be converted to the IR
     * @param literal_type Type information ast to create the the literal
     * type in the IR (only needed for when the expression is a literal).
     * A literal_type is a "typeDescriptions" ast_node.
     * we need this due to some info is missing in the child node.
     * @param new_expr Out parameter to hold the conversion
     * @return true iff the conversion has failed
     * @return false iff the conversion was successful
     */

// FIX ABOVE COMMENTS
//  literal_type is objtype
// expr is the whole expression node
bool clarity_convertert::get_expr(
  const nlohmann::json &expr,
  const nlohmann::json &literal_type,
  exprt &new_expr,
  nlohmann::json &inferred_type)
{
  // For rule expression
  // We need to do location settings to match clang C's number of times to set the locations when recurring

  locationt location;
  get_start_location_from_stmt(expr, location);

  ClarityGrammar::ExpressionT type = ClarityGrammar::get_expression_t(expr);
  log_debug(
    "clarity",
    " @@@ got Expr: ClarityGrammar::ExpressionT::{}",
    ClarityGrammar::expression_to_str(type));

  switch (type)
  {
  case ClarityGrammar::ExpressionT::BinaryOperatorClass:
  {
    nlohmann::json binary_type_expr;
    if (get_binary_operator_expr(expr, new_expr))
      return true;

    if (get_literal_type_from_typet(new_expr.type(), binary_type_expr))
      return true;

    log_debug(
      "clarity",
      " @@@ got Expr type BinaryOperatorClass: typet->objtype::{}",
      binary_type_expr.dump());
    inferred_type.merge_patch(binary_type_expr);
    break;
  }
#if 0
  case ClarityGrammar::ExpressionT::UnaryOperatorClass:
  {
    if (get_unary_operator_expr(expr, literal_type, new_expr))
      return true;
    break;
  }
#endif
  case ClarityGrammar::ExpressionT::ConditionalOperatorClass:
  {
    // for Ternary Operator (...?...:...) only
    if (get_conditional_operator_expr(expr, new_expr))
      return true;

    nlohmann::json conditional_type_expr;
    if (get_literal_type_from_typet(new_expr.type(), conditional_type_expr))
      return true;
    inferred_type.merge_patch(conditional_type_expr);

    break;
  }

  case ClarityGrammar::ExpressionT::DeclRefExprClass:
  {
    // ml- we will be using the cid to find the variable
    int cid = ClarityGrammar::get_experession_cid(expr);
    if (cid > 0)
    {
      // ml- for clarity we will assume that this is a variable declaration always
      nlohmann::json binary_type_expr;
      if (get_var_decl_ref(expr, new_expr))
      {
        return true;
      }
      if (get_literal_type_from_typet(new_expr.type(), binary_type_expr))
        return true;

      inferred_type.merge_patch(binary_type_expr);

      // Go through the symbol table and get the symbol

      // Soldity uses +ve odd numbers to refer to var or functions declared in the contract
      // const nlohmann::json &decl = find_decl_ref(expr["referencedDeclaration"]);
      // if (decl == empty_json)
      //   return true;

      // if (!check_intrinsic_function(decl))
      // {
      //   if (decl["nodeType"] == "VariableDeclaration")
      //   {
      //     if (get_var_decl_ref(decl, new_expr))
      //       return true;
      //   }
      //   else if (decl["nodeType"] == "FunctionDefinition")
      //   {
      //     if (get_func_decl_ref(decl, new_expr))
      //       return true;
      //   }
      //   else if (decl["nodeType"] == "StructDefinition")
      //   {
      //     std::string id;
      //     id = prefix + "struct " + decl["canonicalName"].get<std::string>();

      //     if (context.find_symbol(id) == nullptr)
      //     {
      //       if (get_struct_class(decl))
      //         return true;
      //     }

      //     new_expr = symbol_expr(*context.find_symbol(id));
      //   }
      //   else if (decl["nodeType"] == "ErrorDefinition")
      //   {
      //     std::string name, id;
      //     name = decl["name"].get<std::string>();
      //     id = "clar:@" + name + "#" + std::to_string(decl["id"].get<int>());

      //     if (context.find_symbol(id) == nullptr)
      //       return true;
      //     new_expr = symbol_expr(*context.find_symbol(id));
      //   }
      //   else
      //   {
      //     log_error(
      //       "Unsupported DeclRefExprClass type, got nodeType={}",
      //       decl["nodeType"].get<std::string>());
      //     return true;
      //   }
      // }
      // else
      // {
      //   // for special functions, we need to deal with it separately
      //   if (get_esbmc_builtin_ref(expr, new_expr))
      //     return true;
      // }
    }
    else
    {
      // Soldity uses -ve odd numbers to refer to built-in var or functions that
      // are NOT declared in the contract
      if (get_esbmc_builtin_ref(expr, new_expr))
        return true;
    }

    break;
  }
  case ClarityGrammar::ExpressionT::Literal:
  {
    // make a type-name json for integer literal conversion
    //const nlohmann::json &literal = expr["objtype"];
    nlohmann::json literal_type_expr;
    if (literal_type != nullptr)
    {
      literal_type_expr = literal_type;
    }
    else
    {
      if (!expr.contains("objtype"))
      {
        if (ClarityGrammar::get_literal_type_from_expr(expr, literal_type_expr))
          return true;
        //expr.push_back(nlohmann::json::object_t::value_type("objtype", literal_type_expr));
      }
      else
      {
        //literal_type_expr = expr["objtype"];
        literal_type_expr = ClarityGrammar::get_expression_objtype(expr);
      }
    }

    //if (inferred_type != nullptr)
    inferred_type.merge_patch(literal_type_expr);

    log_status(
      "clarity"
      "	@@@ got Literal type: ClarityGrammar::get_literal_type_from_expr::{}",
      literal_type_expr.dump());
    ClarityGrammar::ElementaryTypeNameT type_name =
      ClarityGrammar::get_elementary_type_name_t(literal_type_expr);
    std::string the_value;

    // if (type_name == ClarityGrammar::ElementaryTypeNameT::PRINCIPAL)
    // {
    //   // for principal literals
    //   the_value = expr[1]["value"][3]["value"][21];
    // }
    // else
    // { //for all other literals

      the_value = ClarityGrammar::get_expression_lit_value(expr);
    //}
    log_debug(
      "clarity",
      "	@@@ got Literal: ClarityGrammar::ElementaryTypeNameT::{}",
      ClarityGrammar::elementary_type_name_to_str(type_name));

    //if (literal_type != nullptr)

    {
      ClarityGrammar::ElementaryTypeNameT type = type_name;

      switch (type_name)
      {
      case ClarityGrammar::ElementaryTypeNameT::BUFF:
      {
        if (
          the_value.length() >= 2 &&
          the_value.substr(0, 2) == "0x") // meaning hex-string
        {
          the_value.erase(0, 2); //remove the first two characters (0x)

          std::string str_buff_size = literal_type_expr[2];
          int buff_size = std::stoi(str_buff_size);

          // Buffer is sort of an array of bytes
          typet type = array_typet(
            unsigned_char_type(), from_integer(buff_size, size_type()));
          exprt buff_inits = gen_zero(type);

          // the string value should be 2xbuff_size long
          // one byte = 2 characters
          int i = 0;
          int value_length = the_value.length();
          for (i = 0; i < value_length / 2; i++)
          {
            unsigned int buff_elem =
              std::stoi(the_value.substr(i * 2, 2), nullptr, 16);
            exprt val = constant_exprt(buff_elem, unsigned_char_type());
            buff_inits.operands().at(i) = val;
          }

          // Handle the case where the last byte is not a complete byte
          if (the_value.length() % 2 != 0)
          {
            std::string last_byte_str =
              the_value.substr(the_value.length() - 1, 1);
            unsigned int last_byte = std::stoi(last_byte_str, nullptr, 16);
            exprt last_byte_expr =
              constant_exprt(last_byte, unsigned_char_type());
            buff_inits.operands().at(i) = last_byte_expr;
          }

          new_expr.swap(buff_inits);
        }
        else
        {
          log_error("Invalid buff value found. Missing 0x prefix");
          return true;
        }
        break;
      }
      case ClarityGrammar::ElementaryTypeNameT::UINT:
      case ClarityGrammar::ElementaryTypeNameT::UINT_LITERAL:
      {
        if (convert_uint_literal(literal_type_expr, the_value, new_expr))
          return true;
        break;
      }
      case ClarityGrammar::ElementaryTypeNameT::INT:
      case ClarityGrammar::ElementaryTypeNameT::INT_LITERAL:
      {
        if (convert_integer_literal(literal_type_expr, the_value, new_expr))
          return true;
        break;
      }
      case ClarityGrammar::ElementaryTypeNameT::BOOL:
      {
        if (convert_bool_literal(literal_type_expr, the_value, new_expr))
          return true;
        break;
      }
      case ClarityGrammar::ElementaryTypeNameT::STRING_ASCII:
      case ClarityGrammar::ElementaryTypeNameT::STRING_ASCII_LITERAL:
      {
        // TODO: figure out how to handle ascii/utf8 strings
        if (convert_string_literal(the_value, new_expr))
          return true;
        break;
      }
      case ClarityGrammar::ElementaryTypeNameT::STRING_UTF8:
      case ClarityGrammar::ElementaryTypeNameT::STRING_UTF8_LITERAL:
      {
        if (convert_string_literal(the_value, new_expr))
          return true;
        break;
      }
      case ClarityGrammar::ElementaryTypeNameT::PRINCIPAL:
      {
        get_principal_instance(expr, new_expr);
        break;
      }
      default:
        assert(!"Error occurred when handling bytes literal");
      }
      break;
    }

    break;
  }
#if 0
  case ClarityGrammar::ExpressionT::Tuple:
  {
    /*
      for each "component" in "objtype[1]"
      1. get the type of the component
      2. get the value of the component
      3. create a new exprt for the component
      4. add the component to the tuple
      5. return the tuple
    
    */

    // 1. construct struct type
    if (get_tuple_definition(expr))
      return true;

    //2. construct struct_type instance
    if (get_tuple_instance(expr, new_expr))
      return true;

    // older code : has useful comments to read
    // assert(expr.contains("components"));

    // switch (type)
    // {
    // // case 1
    // case ClarityGrammar::TypeNameT::ArrayTypeName:
    // {
    //   assert(literal_type != nullptr);

    //   // get elem type
    //   nlohmann::json elem_literal_type =
    //     make_array_elementary_type(literal_type);

    //   // get size
    //   exprt size;
    //   size = constant_exprt(
    //     integer2binary(expr["components"].size(), bv_width(int_type())),
    //     integer2string(expr["components"].size()),
    //     int_type());

    //   // get array type
    //   typet arr_type;
    //   if (get_type_description(literal_type, arr_type))
    //     return true;

    //   // reallocate array size
    //   arr_type = array_typet(arr_type.subtype(), size);

    //   // declare static array tuple
    //   exprt inits;
    //   inits = gen_zero(arr_type);

    //   // populate array
    //   int i = 0;
    //   for (const auto &arg : expr["components"].items())
    //   {
    //     exprt init;
    //     if (get_expr(arg.value(), elem_literal_type, init))
    //       return true;

    //     inits.operands().at(i) = init;
    //     i++;
    //   }

    //   new_expr = inits;
    //   break;
    // }

    // // case 3
    // case ClarityGrammar::TypeNameT::TupleTypeName: // case 3
    // {
    //   /*
    //   we assume there are three types of tuple expr:
    //   0. dump: (x,y);
    //   1. fixed: (x,y) = (y,x);
    //   2. function-related:
    //       2.1. (x,y) = func();
    //       2.2. return (x,y);

    //   case 0:
    //     1. create a struct type
    //     2. create a struct type instance
    //     3. new_expr = instance
    //     e.g.
    //     (x , y) ==>
    //     struct Tuple
    //     {
    //       uint x,
    //       uint y
    //     };
    //     Tuple tuple;

    //   case 1:
    //     1. add special handling in binary operation.
    //        when matching struct_expr A = struct_expr B,
    //        divided into A.operands()[i] = B.operands()[i]
    //        and populated into a code_block.
    //     2. new_expr = code_block
    //     e.g.
    //     (x, y) = (1, 2) ==>
    //     {
    //       tuple.x = 1;
    //       tuple.y = 2;
    //     }
    //     ? any potential scope issue?

    //   case 2:
    //     1. when parsing the funciton definition, if the returnParam > 1
    //        make the function return void instead, and create a struct type
    //     2. when parsing the return statement, if the return value is a tuple,
    //        create a struct type instance, do assignments,  and return empty;
    //     3. when the lhs is tuple and rhs is func_call, get_tuple_instance_expr based
    //        on the func_call, and do case 1.
    //     e.g.
    //     function test() returns (uint, uint)
    //     {
    //       return (1,2);
    //     }
    //     ==>
    //     struct Tuple
    //     {
    //       uint x;
    //       uint y;
    //     }
    //     function test()
    //     {
    //       Tuple tuple;
    //       tuple.x = 1;
    //       tuple.y = 2;
    //       return;
    //     }
    //   */

    //   // 1. construct struct type
    //   if (get_tuple_definition(expr))
    //     return true;

    //   //2. construct struct_type instance
    //   if (get_tuple_instance(expr, new_expr))
    //     return true;

    //   break;
    // }

    // // case 2
    // default:
    // {
    //   if (get_expr(expr["components"][0], literal_type, new_expr))
    //     return true;
    //   break;
    // }
    // }

    break;
  }
  case ClarityGrammar::ExpressionT::Optional:
  {
    get_optional_instance(expr, new_expr);

    break;
  }
  case ClarityGrammar::ExpressionT::Mapping:
  {
    // convert
    //   mapping(string => int) m;
    // to
    //   map_int_t m; map_init(&m);

    // 1. populate the symbol
    exprt dump;
    if (get_var_decl(expr, dump))
      return true;

    // 2. call map_init;
    //TODO
    break;
  }
  case ClarityGrammar::ExpressionT::CallExprClass:
  {
    const nlohmann::json &callee_expr_json = expr["expression"];

    // 0. check if it's a clarity built-in function
    if (
      !get_clar_builtin_ref(expr, new_expr) &&
      !check_intrinsic_function(callee_expr_json))
    {
      // construct call
      typet type = to_code_type(new_expr.type()).return_type();

      side_effect_expr_function_callt call;
      call.function() = new_expr;
      call.type() = type;

      // populate params
      // the number of arguments defined in the template
      size_t define_size = to_code_type(new_expr.type()).arguments().size();
      // the number of arguments actually inside the json file
      const size_t arg_size = expr["arguments"].size();
      if (define_size >= arg_size)
      {
        // we only populate the exact number of args according to the template
        for (const auto &arg : expr["arguments"].items())
        {
          exprt single_arg;
          if (get_expr(
                arg.value(), arg.value()["typeDescriptions"], single_arg))
            return true;

          call.arguments().push_back(single_arg);
        }
      }

      new_expr = call;
      break;
    }

    // 1. Get callee expr
    if (
      callee_expr_json.contains("nodeType") &&
      callee_expr_json["nodeType"] == "MemberAccess")
    {
      // ContractMemberCall

      const int contract_func_id =
        callee_expr_json["referencedDeclaration"].get<int>();
      const nlohmann::json caller_expr_json = find_decl_ref(contract_func_id);
      if (caller_expr_json == empty_json)
        return true;

      std::string ref_contract_name;
      if (get_current_contract_name(caller_expr_json, ref_contract_name))
        return true;

      std::string name, id;
      get_function_definition_name(caller_expr_json, name, id);

      if (context.find_symbol(id) == nullptr)
        // probably a built-in function
        // that is not supported yet
        return true;

      const symbolt s = *context.find_symbol(id);
      typet type = s.type;

      new_expr = exprt("symbol", type);
      new_expr.identifier(id);
      new_expr.cmt_lvalue(true);
      new_expr.name(name);
      new_expr.set("#member_name", prefix + ref_contract_name);

      // obtain the type of return value
      // It can be retrieved directly from the original function declaration
      typet t;
      if (get_type_description(caller_expr_json["returnParameters"], t))
        return true;

      side_effect_expr_function_callt call;
      call.function() = new_expr;
      call.type() = t;

      // populate params
      auto param_nodes = caller_expr_json["parameters"]["parameters"];
      unsigned num_args = 0;
      nlohmann::json param = nullptr;
      nlohmann::json::iterator itr = param_nodes.begin();

      for (const auto &arg : expr["arguments"].items())
      {
        if (itr != param_nodes.end())
        {
          if ((*itr).contains("typeDescriptions"))
          {
            param = (*itr)["typeDescriptions"];
          }
          ++itr;
        }

        exprt single_arg;
        if (get_expr(arg.value(), param, single_arg))
          return true;

        call.arguments().push_back(single_arg);
        ++num_args;
        param = nullptr;
      }

      new_expr = call;
      break;
    }

    // wrap it in an ImplicitCastExpr to perform conversion of FunctionToPointerDecay
    nlohmann::json implicit_cast_expr =
      make_implicit_cast_expr(callee_expr_json, "FunctionToPointerDecay");
    exprt callee_expr;
    if (get_expr(implicit_cast_expr, callee_expr))
      return true;

    // 2. Get type
    // extract from the return_type
    assert(callee_expr.is_symbol());
    if (expr["kind"] == "structConstructorCall")
    {
      // e.g. Book book = Book('Learn Java', 'TP', 1);
      if (callee_expr.type().id() != irept::id_struct)
        return true;

      typet t = callee_expr.type();
      exprt inits = gen_zero(t);

      int ref_id = callee_expr_json["referencedDeclaration"].get<int>();
      const nlohmann::json struct_ref = find_decl_ref(ref_id);
      if (struct_ref == empty_json)
        return true;

      const nlohmann::json members = struct_ref["members"];
      const nlohmann::json args = expr["arguments"];

      // popluate components
      for (size_t i = 0; i < inits.operands().size() && i < args.size(); i++)
      {
        exprt init;
        if (get_expr(args.at(i), members.at(i)["typeDescriptions"], init))
          return true;

        const struct_union_typet::componentt *c =
          &to_struct_type(t).components().at(i);
        typet elem_type = c->type();

        clarity_gen_typecast(ns, init, elem_type);
        inits.operands().at(i) = init;
      }

      new_expr = inits;
      break;
    }

    // funciton call expr
    assert(callee_expr.type().is_code());
    typet type = to_code_type(callee_expr.type()).return_type();

    side_effect_expr_function_callt call;
    call.function() = callee_expr;
    call.type() = type;

    // special case: handling revert and require
    // insert a bool false as the first argument.
    // drop the rest of params.
    if (
      callee_expr.type().get("#clar_name").as_string().find("revert") !=
      std::string::npos)
    {
      call.arguments().push_back(false_exprt());
      new_expr = call;

      break;
    }

    // 3. populate param
    assert(callee_expr_json.contains("referencedDeclaration"));

    //! we might use int_const instead of the original param type (e.g. uint_8).
    nlohmann::json param_nodes = callee_expr_json["argumentTypes"];
    nlohmann::json param = nullptr;
    nlohmann::json::iterator itr = param_nodes.begin();
    unsigned num_args = 0;

    for (const auto &arg : expr["arguments"].items())
    {
      exprt single_arg;
      if (get_expr(arg.value(), *itr, single_arg))
        return true;
      call.arguments().push_back(single_arg);

      ++num_args;
      ++itr;
      param = nullptr;

      // Special case: require
      // __ESBMC_assume only handle one param.
      if (
        callee_expr.type().get("#clar_name").as_string().find("require") !=
        std::string::npos)
        break;
    }
    log_debug("clarity", "  @@ num_args={}", num_args);

    new_expr = call;
    break;
  }
  case ClarityGrammar::ExpressionT::ImplicitCastExprClass:
  {
    if (get_cast_expr(expr, new_expr, literal_type))
      return true;
    break;
  }
  case ClarityGrammar::ExpressionT::IndexAccess:
  {
    // 1. get type, this is the base type of array
    typet t;
    if (get_type_description(expr["typeDescriptions"], t))
      return true;

    // for BYTESN, where the index access is read-only
    if (is_bytes_type(t))
    {
      // this means we are dealing with bytes type
      // jump out if it's "bytes[]" or "bytesN[]" or "func()[]"
      ClarityGrammar::TypeNameT tname = ClarityGrammar::get_type_name_t(
        expr["baseExpression"]["typeDescriptions"]);
      if (
        !(tname == ClarityGrammar::ListTypeName) &&
        expr["baseExpression"].contains("referencedDeclaration"))
      {
        // e.g.
        //    bytes3 x = 0x123456
        //    bytes1 y = x[0]; // 0x12
        //    bytes1 z = x[1]; // 0x34
        // which equals to
        //    bytes1 z = bswap(x) >> 1 & 0xff
        // for bytes32 x = "test";
        //    x[10] == 0x00 due to the padding
        exprt src_val, src_offset, bswap, bexpr;

        const nlohmann::json &decl = find_decl_ref(
          expr["baseExpression"]["referencedDeclaration"].get<int>());
        if (decl == empty_json)
          return true;

        if (get_var_decl_ref(decl, src_val))
          return true;

        if (get_expr(
              expr["indexExpression"], expr["typeDescriptions"], src_offset))
          return true;

        // extract particular byte based on idx (offset)
        bexpr = exprt("byte_extract_big_endian", src_val.type());
        bexpr.copy_to_operands(src_val, src_offset);

        clarity_gen_typecast(ns, bexpr, unsignedbv_typet(8));

        new_expr = bexpr;
        break;
      }
    }

    // 2. get the decl ref of the array

    exprt array;

    // 2.1 arr[n] / x.arr[n]
    if (expr["baseExpression"].contains("referencedDeclaration"))
    {
      if (get_expr(expr["baseExpression"], literal_type, array))
        return true;
    }
    else
    {
      // 2.2 func()[n]
      const nlohmann::json &decl = expr["baseExpression"];
      nlohmann::json implicit_cast_expr =
        make_implicit_cast_expr(decl, "ArrayToPointerDecay");
      if (get_expr(implicit_cast_expr, literal_type, array))
        return true;
    }

    // 3. get the position index
    exprt pos;
    if (get_expr(expr["indexExpression"], expr["typeDescriptions"], pos))
      return true;

    // BYTES:  func_ret_bytes()[]
    // same process as above
    if (is_bytes_type(array.type()))
    {
      exprt bexpr = exprt("byte_extract_big_endian", pos.type());
      bexpr.copy_to_operands(array, pos);
      clarity_gen_typecast(ns, bexpr, unsignedbv_typet(8));
      new_expr = bexpr;
      break;
    }

    new_expr = index_exprt(array, pos, t);
    break;
  }
  case ClarityGrammar::ExpressionT::NewExpression:
  {
    // 1. new dynamic array, e.g.
    //    uint[] memory a = new uint[](7);
    // 2. new bytes array e.g.
    //    bytes memory b = new bytes(7)
    // 3. new object, e.g.
    //    Base x = new Base(1, 2);

    // case 1
    // e.g.
    //    a = new uint[](7)
    // convert to
    //    uint y[7] = {0,0,0,0,0,0,0};
    //    a = y;
    nlohmann::json callee_expr_json = expr["expression"];
    if (callee_expr_json.contains("typeName"))
    {
      // case 1
      // e.g.
      //    a = new uint[](7)
      // convert to
      //    uint y[7] = {0,0,0,0,0,0,0};
      //    a = y;
      if (is_dyn_array(callee_expr_json["typeName"]["typeDescriptions"]))
      {
        if (get_empty_array_ref(expr, new_expr))
          return true;
        break;
      }
      //case 2:
      // the contract/constructor name cannot be "bytes"
      if (
        callee_expr_json["typeName"]["typeDescriptions"]["typeString"]
          .get<std::string>() == "bytes")
      {
        // populate 0x00 to bytes array
        // same process in case ClarityGrammar::ExpressionT::Literal
        assert(expr.contains("arguments") && expr["arguments"].size() == 1);

        int byte_size = stoi(expr["arguments"][0]["value"].get<std::string>());
        std::string hex_val = "";

        for (int i = 0; i < byte_size; i++)
          hex_val += "00";
        hex_val.resize(byte_size * 2);

        if (convert_hex_literal(hex_val, new_expr, byte_size * 8))
          return true;
        break;
      }
    }

    // case 3
    // first, call the constructor
    if (get_constructor_call(expr, new_expr))
      return true;

    side_effect_exprt tmp_obj("temporary_object", new_expr.type());
    codet code_expr("expression");
    code_expr.operands().push_back(new_expr);
    tmp_obj.initializer(code_expr);
    tmp_obj.location() = new_expr.location();
    new_expr.swap(tmp_obj);

    break;
  }
  case ClarityGrammar::ExpressionT::ContractMemberCall:
  case ClarityGrammar::ExpressionT::StructMemberCall:

  case ClarityGrammar::ExpressionT::List:
  {
    get_list_of_entry_type(expr, new_expr);
    break;
  }
  case ClarityGrammar::ExpressionT::BuiltinMemberCall:
  {
    if (get_clar_builtin_ref(expr, new_expr))
      return true;
    break;
  }
  case ClarityGrammar::ExpressionT::ElementaryTypeNameExpression:
  {
    // perform type conversion
    // e.g.
    // address payable public owner = payable(msg.sender);
    // or
    // uint32 a = 0x432178;
    // uint16 b = uint16(a); // b will be 0x2178 now

    assert(expr.contains("expression"));
    const nlohmann::json conv_expr = expr["expression"];
    typet type;
    exprt from_expr;

    // 1. get source expr
    // assume: only one argument
    if (get_expr(expr["arguments"][0], literal_type, from_expr))
      return true;

    // 2. get target type
    if (get_type_description(conv_expr["typeDescriptions"], type))
      return true;

    // 3. generate the type casting expr
    convert_type_expr(ns, from_expr, type);

    new_expr = from_expr;
    break;
  }
#endif
  case ClarityGrammar::ExpressionT::NullExpr:
  {
    // e.g. (, x) = (1, 2);
    // the first component in lhs is nil
    new_expr = nil_exprt();
    break;
  }
  default:
  {
    assert(!"Unimplemented type in rule expression");
    return true;
  }
  }

  new_expr.location() = location;
  return false;
}

void clarity_convertert::set_current_contract_name(std::string &contract_name)
{
  current_contractName = contract_name;
}

bool clarity_convertert::get_current_contract_name(
  const nlohmann::json &ast_node,
  std::string &contract_name)
{
  contract_name = current_contractName;
  return false;

  // unexpected
  return true;
}

bool clarity_convertert::get_binary_operator_expr(
  const nlohmann::json &expr,
  exprt &new_expr)
{
  // 1. Convert LHS and RHS
  // For "Assignment" expression, it's called "leftHandSide" or "rightHandSide".
  // For "BinaryOperation" expression, it's called "leftExpression" or "leftExpression"
  exprt lhs, rhs;
  // if (expr.contains("leftHandSide"))
  // {
  //   nlohmann::json literalType = expr["leftHandSide"]["typeDescriptions"];

  //   if (get_expr(expr["leftHandSide"], lhs))
  //     return true;

  //   if (get_expr(expr["rightHandSide"], literalType, rhs))
  //     return true;
  // }
  // else if (expr.contains("leftExpression"))
  // {
  //   nlohmann::json literalType_l = expr["leftExpression"]["typeDescriptions"];
  //   nlohmann::json literalType_r = expr["rightExpression"]["typeDescriptions"];
  nlohmann::json type_l;
  nlohmann::json type_r;
  nlohmann::json exp_args = ClarityGrammar::get_expression_args(expr);

  if (get_expr(exp_args[0], nullptr, lhs, type_l))
    return true;

  if (get_expr(exp_args[1], nullptr, rhs, type_r))
    return true;

  // ml-[TODO] need to check compatibility of the two expressions
  // }
  // else
  //   assert(!"should not be here - unrecognized LHS and RHS keywords in expression JSON");

  // preliminary step for recursive BinaryOperation
  current_BinOp_type.push(&(type_l));

  // 2. Get type
  typet t;
  assert(current_BinOp_type.size());
  const nlohmann::json &binop_type = *(current_BinOp_type.top());
  if (get_type_description(binop_type, t))
    return true;

  typet common_type;
  if (expr.contains("commonType"))
  {
    if (get_type_description(expr["commonType"], common_type))
      return true;
  }

  // 3. Convert opcode
  ClarityGrammar::ExpressionT opcode =
    ClarityGrammar::get_expr_operator_t(expr);
  log_debug(
    "clarity",
    "	@@@ got binop.getOpcode: ClarityGrammar::{}",
    ClarityGrammar::expression_to_str(opcode));

  switch (opcode)
  {
  case ClarityGrammar::ExpressionT::BO_Assign:
  {
    // special handling for tuple-type assignment;
    typet lt = lhs.type();
    typet rt = rhs.type();
    if (lt.get("#clar_type") == "tuple_instance")
    {
      code_blockt _block;
      if (rt.get("#clar_type") == "tuple_instance")
      {
        // e.g. (x,y) = (1,2); (x,y) = (func(),x);
        // =>
        //  t.mem0 = 1; #1
        //  t.mem1 = 2; #2
        //  x = t.mem0; #3
        //  y = t.mem1; #4

        size_t i = 0;
        size_t j = 0;
        size_t ls = to_struct_type(lhs.type()).components().size();
        size_t rs = to_struct_type(rhs.type()).components().size();
        assert(ls <= rs);

        // do #1 #2
        while (i < rs)
        {
          exprt lop;
          if (get_tuple_member_call(
                rhs.identifier(),
                to_struct_type(rhs.type()).components().at(i),
                lop))
            return true;

          exprt rop = rhs.operands().at(i);
          //do assignment
          get_tuple_assignment(_block, lop, rop);
          // update counter
          ++i;
        }

        // reset
        i = 0;

        // do #3 #4
        while (i < ls && j < rs)
        {
          // construct assignemnt
          exprt lcomp = to_struct_type(lhs.type()).components().at(i);
          exprt rcomp = to_struct_type(rhs.type()).components().at(j);
          exprt lop = lhs.operands().at(i);
          exprt rop;

          if (get_tuple_member_call(
                rhs.identifier(),
                to_struct_type(rhs.type()).components().at(j),
                rop))
            return true;

          if (lcomp.name() != rcomp.name())
          {
            // e.g. (, x) = (1, 2)
            //        null <=> tuple2.mem0
            // tuple1.mem1 <=> tuple2.mem1
            ++j;
            continue;
          }
          //do assignment
          get_tuple_assignment(_block, lop, rop);
          // update counter
          ++i;
          ++j;
        }
      }
      else if (rt.get("#clar_type") == "tuple")
      {
        // e.g. (x,y) = func(); (x,y) = func(func2()); (x, (x,y)) = (x, func());
        exprt new_rhs;
        if (get_tuple_function_ref(
              expr["rightHandSide"]["expression"], new_rhs))
          return true;

        // add function call
        get_tuple_function_call(_block, rhs);

        size_t ls = to_struct_type(lhs.type()).components().size();
        size_t rs = to_struct_type(new_rhs.type()).components().size();
        assert(ls == rs);

        for (size_t i = 0; i < ls; i++)
        {
          exprt lop = lhs.operands().at(i);

          exprt rop;
          if (get_tuple_member_call(
                new_rhs.identifier(),
                to_struct_type(new_rhs.type()).components().at(i),
                rop))
            return true;

          get_tuple_assignment(_block, lop, rop);
        }
      }
      else
      {
        log_error("Unexpected Tuple");
        abort();
      }

      // fix ordering
      // e.g.
      // x = 1;
      // y = 2;
      // (x , y , x, y) =(y, x , 0, 0);
      // assert(x == 2); // hold
      // assert(y == 1); // hold
      code_blockt ordered_block;
      std::set<irep_idt> assigned_symbol;
      for (auto &assign : _block.operands())
      {
        // assume lhs should always be a symbol type
        irep_idt id = assign.op0().op0().identifier();
        if (!id.empty() && assigned_symbol.count(id))
          // e.g. (x,x) = (1, 2); x==1 hold
          continue;
        assigned_symbol.insert(id);
        ordered_block.move_to_operands(assign);
      }

      new_expr = ordered_block;

      current_BinOp_type.pop();
      return false;
    }

    new_expr = side_effect_exprt("assign", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Add:
  {
    if (t.is_floatbv())
      assert(!"Clarity does not support FP arithmetic as of v0.8.6.");
    else
      new_expr = exprt("+", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Sub:
  {
    if (t.is_floatbv())
      assert(!"Clarity does not support FP arithmetic as of v0.8.6.");
    else
      new_expr = exprt("-", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Mul:
  {
    if (t.is_floatbv())
      assert(!"Clarity does not support FP arithmetic as of v0.8.6.");
    else
      new_expr = exprt("*", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Div:
  {
    if (t.is_floatbv())
      assert(!"Clarity does not support FP arithmetic as of v0.8.6.");
    else
      new_expr = exprt("/", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Rem:
  {
    new_expr = exprt("mod", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Shl:
  {
    new_expr = exprt("shl", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Shr:
  {
    new_expr = exprt("shr", t);
    break;
  }
  case ClarityGrammar::BO_And:
  {
    new_expr = exprt("bitand", t);
    break;
  }
  case ClarityGrammar::BO_Xor:
  {
    new_expr = exprt("bitxor", t);
    break;
  }
  case ClarityGrammar::BO_Or:
  {
    new_expr = exprt("bitor", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_GT:
  {
    new_expr = exprt(">", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_LT:
  {
    new_expr = exprt("<", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_GE:
  {
    new_expr = exprt(">=", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_LE:
  {
    new_expr = exprt("<=", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_NE:
  {
    new_expr = exprt("notequal", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_EQ:
  {
    new_expr = exprt("=", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_LAnd:
  {
    new_expr = exprt("and", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_LOr:
  {
    new_expr = exprt("or", t);
    break;
  }
  case ClarityGrammar::ExpressionT::BO_Pow:
  {
    // lhs**rhs => pow(lhs, rhs)
    // double pow(double base, double exponent)

    side_effect_expr_function_callt call_expr;

    exprt type_expr("symbol");
    type_expr.name("pow");
    type_expr.identifier("c:@F@pow");
    type_expr.location() = lhs.location();

    code_typet type;
    type.return_type() = double_type();
    type_expr.type() = type;

    call_expr.function() = type_expr;
    call_expr.type() = double_type();
    call_expr.set("#cpp_type", "double");

    clarity_gen_typecast(ns, lhs, double_type());
    clarity_gen_typecast(ns, rhs, double_type());
    call_expr.arguments().push_back(lhs);
    call_expr.arguments().push_back(rhs);

    new_expr = call_expr;

    return false;
  }
  default:
  {
    if (get_compound_assign_expr(expr, new_expr))
    {
      assert(!"Unimplemented binary operator");
      return true;
    }

    current_BinOp_type.pop();

    return false;
  }
  }

  // for bytes type
  if (is_bytes_type(lhs.type()) || is_bytes_type(rhs.type()))
  {
    switch (opcode)
    {
    case ClarityGrammar::ExpressionT::BO_GT:
    case ClarityGrammar::ExpressionT::BO_LT:
    case ClarityGrammar::ExpressionT::BO_GE:
    case ClarityGrammar::ExpressionT::BO_LE:
    case ClarityGrammar::ExpressionT::BO_NE:
    case ClarityGrammar::ExpressionT::BO_EQ:
    {
      // e.g. cmp(0x74,  0x7500)
      // ->   cmp(0x74, 0x0075)

      // convert string to bytes
      // e.g.
      //    data1 = "test"; data2 = 0x74657374; // "test"
      //    assert(data1 == data2); // true
      // Do type conversion before the bswap
      // the arguement of bswap should only be int/uint type, not string
      // e.g. data1 == "test", it should not be bswap("test")
      // instead it should be bswap(0x74657374)
      // this may overwrite the lhs & rhs.
      if (!is_bytes_type(lhs.type()))
      {
        if (get_expr(expr["leftExpression"], expr["commonType"], lhs))
          return true;
      }
      if (!is_bytes_type(rhs.type()))
      {
        if (get_expr(expr["rightExpression"], expr["commonType"], rhs))
          return true;
      }

      // do implicit type conversion
      convert_type_expr(ns, lhs, common_type);
      convert_type_expr(ns, rhs, common_type);

      exprt bwrhs, bwlhs;
      bwlhs = exprt("bswap", common_type);
      bwlhs.operands().push_back(lhs);
      lhs = bwlhs;

      bwrhs = exprt("bswap", common_type);
      bwrhs.operands().push_back(rhs);
      rhs = bwrhs;

      new_expr.copy_to_operands(lhs, rhs);
      // Pop current_BinOp_type.push as we've finished this conversion
      current_BinOp_type.pop();
      return false;
    }
    case ClarityGrammar::ExpressionT::BO_Shl:
    {
      // e.g.
      //    bytes1 = 0x11
      //    x<<8 == 0x00
      new_expr.copy_to_operands(lhs, rhs);
      convert_type_expr(ns, new_expr, lhs.type());
      current_BinOp_type.pop();
      return false;
    }
    default:
    {
      break;
    }
    }
  }

  // 4.1 check if it needs implicit type conversion
  if (common_type.id() != "")
  {
    convert_type_expr(ns, lhs, common_type);
    convert_type_expr(ns, rhs, common_type);
  }

  // 4.2 Copy to operands
  new_expr.copy_to_operands(lhs, rhs);

  // Pop current_BinOp_type.push as we've finished this conversion
  current_BinOp_type.pop();

  return false;
}

bool clarity_convertert::get_compound_assign_expr(
  const nlohmann::json &expr,
  exprt &new_expr)
{
  // equivalent to clang_c_convertert::get_compound_assign_expr

  ClarityGrammar::ExpressionT opcode =
    ClarityGrammar::get_expr_operator_t(expr);

  switch (opcode)
  {
  case ClarityGrammar::ExpressionT::BO_AddAssign:
  {
    new_expr = side_effect_exprt("assign+");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_SubAssign:
  {
    new_expr = side_effect_exprt("assign-");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_MulAssign:
  {
    new_expr = side_effect_exprt("assign*");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_DivAssign:
  {
    new_expr = side_effect_exprt("assign_div");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_RemAssign:
  {
    new_expr = side_effect_exprt("assign_mod");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_ShlAssign:
  {
    new_expr = side_effect_exprt("assign_shl");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_ShrAssign:
  {
    new_expr = side_effect_exprt("assign_shr");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_AndAssign:
  {
    new_expr = side_effect_exprt("assign_bitand");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_XorAssign:
  {
    new_expr = side_effect_exprt("assign_bitxor");
    break;
  }
  case ClarityGrammar::ExpressionT::BO_OrAssign:
  {
    new_expr = side_effect_exprt("assign_bitor");
    break;
  }
  default:
    return true;
  }

  exprt lhs, rhs;
  if (expr.contains("leftHandSide"))
  {
    nlohmann::json literalType = expr["leftHandSide"]["typeDescriptions"];

    if (get_expr(expr["leftHandSide"], lhs))
      return true;

    if (get_expr(expr["rightHandSide"], literalType, rhs))
      return true;
  }
  else if (expr.contains("leftExpression"))
  {
    nlohmann::json literalType_l = expr["leftExpression"]["typeDescriptions"];
    nlohmann::json literalType_r = expr["rightExpression"]["typeDescriptions"];

    if (get_expr(expr["leftExpression"], literalType_l, lhs))
      return true;

    if (get_expr(expr["rightExpression"], literalType_r, rhs))
      return true;
  }
  else
    assert(!"should not be here - unrecognized LHS and RHS keywords in expression JSON");

  assert(current_BinOp_type.size());
  const nlohmann::json &binop_type = *(current_BinOp_type.top());
  if (get_type_description(binop_type, new_expr.type()))
    return true;

  typet common_type;
  if (expr.contains("commonType"))
  {
    if (get_type_description(expr["commonType"], common_type))
      return true;
  }

  if (common_type.id() != "")
  {
    convert_type_expr(ns, lhs, common_type);
    convert_type_expr(ns, rhs, common_type);
  }

  new_expr.copy_to_operands(lhs, rhs);
  return false;
}

bool clarity_convertert::get_unary_operator_expr(
  const nlohmann::json &expr,
  const nlohmann::json &literal_type,
  exprt &new_expr)
{
  // TODO: Fix me! Currently just support prefix == true,e.g. pre-increment

  // 1. get UnaryOperation opcode
  ClarityGrammar::ExpressionT opcode =
    ClarityGrammar::get_unary_expr_operator_t(expr, expr["prefix"]);
  log_debug(
    "clarity",
    "	@@@ got uniop.getOpcode: ClarityGrammar::{}",
    ClarityGrammar::expression_to_str(opcode));

  // 2. get type
  typet uniop_type;
  if (get_type_description(expr["typeDescriptions"], uniop_type))
    return true;

  // 3. get subexpr
  exprt unary_sub;
  if (get_expr(expr["subExpression"], literal_type, unary_sub))
    return true;

  switch (opcode)
  {
  case ClarityGrammar::ExpressionT::UO_PreDec:
  {
    new_expr = side_effect_exprt("predecrement", uniop_type);
    break;
  }
  case ClarityGrammar::ExpressionT::UO_PreInc:
  {
    new_expr = side_effect_exprt("preincrement", uniop_type);
    break;
  }
  case ClarityGrammar::UO_PostDec:
  {
    new_expr = side_effect_exprt("postdecrement", uniop_type);
    break;
  }
  case ClarityGrammar::UO_PostInc:
  {
    new_expr = side_effect_exprt("postincrement", uniop_type);
    break;
  }
  case ClarityGrammar::ExpressionT::UO_Minus:
  {
    new_expr = exprt("unary-", uniop_type);
    break;
  }
  case ClarityGrammar::ExpressionT::UO_Not:
  {
    new_expr = exprt("bitnot", uniop_type);
    break;
  }

  case ClarityGrammar::ExpressionT::UO_LNot:
  {
    new_expr = exprt("not", bool_type());
    break;
  }
  default:
  {
    assert(!"Unimplemented unary operator");
  }
  }

  new_expr.operands().push_back(unary_sub);
  return false;
}

bool clarity_convertert::get_conditional_operator_expr(
  const nlohmann::json &expr,
  exprt &new_expr)
{
  nlohmann::json args;
  nlohmann::json condition_expr;
  nlohmann::json true_expr;
  nlohmann::json false_expr;

  // ml- for conditional operation the args[0] contains the
  //     conditions expression
  args = ClarityGrammar::get_expression_args(expr);
  
  // check that there should be at least 1 element in args
  if (args.is_array() && args.size() > 0)
  {
    condition_expr = args[0];
  }
  else
  {
    log_debug(
      "clarity",
      "	@@@ get_conditional_operator_expr args are not array or less than 0 {}",
      args.dump());
    return true;
  }

  // ml- for conditional operation the args[1] contains the
  //     true expression and args[2] contains the false
  //     expression. There can be a case where there is only
  //     a true expression. in that case make the false expr
  //     as nop
  if (args.size() > 1)
  {
    true_expr = args[1];
    false_expr = args.size() > 2 ? args[2] : false_expr;
  }
  else
  {
    log_debug(
      "clarity",
      "	@@@ get_conditional_operator_expr args less than 1 {}",
      args.dump());
    return true;
  }

  exprt cond;
  if (get_expr(condition_expr, cond))
    return true;

  exprt then;
  nlohmann::json then_type;
  if (get_expr(true_expr, nullptr, then, then_type))
    return true;

  exprt else_expr;
  nlohmann::json else_type;
  if (!false_expr.is_null())
  {
    if (get_expr(false_expr, nullptr, else_expr, else_type))
      return true;
  }
  else
  {
    // empty expression
    else_expr = nil_exprt();
  }

  // ml-[TODO] check that the two if and else types are same
  typet t;
  if (get_type_description(then_type, t))
    return true;

  // ml-[TODO] implement assert later
  exprt if_expr("if", t);
  if_expr.copy_to_operands(cond, then, else_expr);

  new_expr = if_expr;

  return false;
}

bool clarity_convertert::get_cast_expr(
  const nlohmann::json &cast_expr,
  exprt &new_expr,
  const nlohmann::json literal_type)
{
  // 1. convert subexpr
  exprt expr;
  if (get_expr(cast_expr["subExpr"], literal_type, expr))
    return true;

  // 2. get type
  typet type;
  if (cast_expr["castType"].get<std::string>() == "ArrayToPointerDecay")
  {
    // Array's cast_expr will have cast_expr["subExpr"]["typeDescriptions"]:
    //  "typeIdentifier": "t_array$_t_uint8_$2_memory_ptr"
    //  "typeString": "uint8[2] memory"
    // For the data above, ClarityGrammar::get_type_name_t will return ArrayTypeName.
    // But we want Pointer type. Hence, adjusting the type manually to make it like:
    //   "typeIdentifier": "ArrayToPtr",
    //   "typeString": "uint8[2] memory"
    nlohmann::json adjusted_type =
      make_array_to_pointer_type(cast_expr["subExpr"]["typeDescriptions"]);
    if (get_type_description(adjusted_type, type))
      return true;
  }
  // TODO: Maybe can just type = expr.type() for other types as well. Need to make sure types are all set in get_expr (many functions are called multiple times to perform the same action).
  else
  {
    type = expr.type();
  }

  // 3. get cast type and generate typecast
  ClarityGrammar::ImplicitCastTypeT cast_type =
    ClarityGrammar::get_implicit_cast_type_t(
      cast_expr["castType"].get<std::string>());
  switch (cast_type)
  {
  case ClarityGrammar::ImplicitCastTypeT::LValueToRValue:
  {
    clarity_gen_typecast(ns, expr, type);
    break;
  }
  case ClarityGrammar::ImplicitCastTypeT::FunctionToPointerDecay:
  case ClarityGrammar::ImplicitCastTypeT::ArrayToPointerDecay:
  {
    break;
  }
  default:
  {
    assert(!"Unimplemented implicit cast type");
  }
  }

  new_expr = expr;
  return false;
}

bool clarity_convertert::get_var_decl_ref(
  const nlohmann::json &decl,
  exprt &new_expr)
{
  // Function to configure new_expr that has a +ve referenced id, referring to a variable declaration
  std::string decl_type = ClarityGrammar::get_expression_type(decl);
  assert(decl_type == "variable");
  std::string name, id;
  std::string state_name, state_id;
  bool non_state_found = false;
  get_state_var_decl_name(decl, state_name, state_id);
  if (current_functionDecl != nullptr)
  {
    get_var_decl_name(decl, name, id);
    log_status(
      "clarity"
      "	@@@ get_var_decl_ref finding id as function var::{}",
      id);

    if (context.find_symbol(id) != nullptr)
    {
      new_expr = symbol_expr(*context.find_symbol(id));
      non_state_found = true;
    }
  }
  if (!non_state_found)
  {
    log_status(
      "clarity"
      "	@@@ get_var_decl_ref finding state id as function var::{}",
      id);

    if (context.find_symbol(state_id) != nullptr)
      new_expr = symbol_expr(*context.find_symbol(state_id));
    else
      return true;
  }
  // else
  // {
  //   typet type;
  //   if (get_type_description(
  //         decl["typeName"]["typeDescriptions"],
  //         type)) // "type-name" as in state-variable-declaration
  //     return true;

  //   new_expr = exprt("symbol", type);
  //   new_expr.identifier(id);
  //   new_expr.cmt_lvalue(true);
  //   new_expr.name(name);
  //   new_expr.pretty_name(name);
  // }

  return false;
}

bool clarity_convertert::get_func_decl_ref(
  const nlohmann::json &decl,
  exprt &new_expr)
{
  // Function to configure new_expr that has a +ve referenced id, referring to a function declaration
  // This allow to get func symbol before we add it to the symbol table
  assert(decl["nodeType"] == "FunctionDefinition");
  std::string name, id;
  get_function_definition_name(decl, name, id);

  typet type;
  if (get_func_decl_ref_type(
        decl, type)) // "type-name" as in state-variable-declaration
    return true;

  new_expr = exprt("symbol", type);
  new_expr.identifier(id);
  new_expr.cmt_lvalue(true);
  new_expr.name(name);
  return false;
}

bool clarity_convertert::get_enum_member_ref(
  const nlohmann::json &decl,
  exprt &new_expr)
{
  assert(decl["nodeType"] == "EnumValue");
  assert(decl.contains("Value"));

  const std::string val = decl["Value"].get<std::string>();

  new_expr = constant_exprt(
    integer2binary(string2integer(val), bv_width(int_type())), val, int_type());

  return false;
}

// get the esbmc built-in methods
bool clarity_convertert::get_esbmc_builtin_ref(
  const nlohmann::json &decl,
  exprt &new_expr)
{
  // Function to configure new_expr that has a -ve referenced id
  // -ve ref id means built-in functions or variables.
  // Add more special function names here

  assert(decl.contains("name"));
  const std::string blt_name = decl["name"].get<std::string>();
  std::string name, id;

  // "require" keyword is virtually identical to "assume"
  if (blt_name == "require" || blt_name == "revert")
    name = "__ESBMC_assume";
  else
    name = blt_name;
  id = name;

  // manually unrolled recursion here
  // type config for Builtin && Int
  typet type;
  // Creat a new code_typet, parse the return_type and copy the code_typet to typet
  code_typet convert_type;
  typet return_type;
  if (
    name == "assert" || name == "__ESBMC_assume" || name == "__VERIFIER_assume")
  {
    // clang's assert(.) uses "signed_int" as assert(.) type (NOT the argument type),
    // while Clarity's assert uses "bool" as assert(.) type (NOT the argument type).
    return_type = bool_type();
    std::string c_type = "bool";
    return_type.set("#cpp_type", c_type);
    convert_type.return_type() = return_type;

    if (!convert_type.arguments().size())
      convert_type.make_ellipsis();
  }
  else
  {
    //!assume it's a clarity built-in func
    return get_clar_builtin_ref(decl, new_expr);
  }

  type = convert_type;
  type.set("#clar_name", blt_name);

  new_expr = exprt("symbol", type);
  new_expr.identifier(id);
  new_expr.cmt_lvalue(true);
  new_expr.name(name);

  return false;
}

/*
  check if it's a clarity built-in function
  - if so, get the function definition reference, assign to new_expr and return false
  - if not, return true
*/
bool clarity_convertert::get_clar_builtin_ref(
  const nlohmann::json expr,
  exprt &new_expr)
{
  // get the reference from the pre-populated symbol table
  // note that this could be either vars or funcs.
  assert(expr.contains("nodeType"));

  if (expr["nodeType"].get<std::string>() == "FunctionCall")
  {
    //  e.g. gasleft() <=> c:@gasleft
    if (expr["expression"]["nodeType"].get<std::string>() != "Identifier")
      // this means it's not a builtin funciton
      return true;

    std::string name = expr["expression"]["name"].get<std::string>();
    std::string id = "c:@F@" + name;
    if (context.find_symbol(id) == nullptr)
      return true;
    const symbolt &sym = *context.find_symbol(id);
    new_expr = symbol_expr(sym);
  }
  else if (expr["nodeType"].get<std::string>() == "MemberAccess")
  {
    // e.g. string.concat() <=> c:@string_concat
    std::string bs;
    if (expr["expression"].contains("name"))
      bs = expr["expression"]["name"].get<std::string>();
    else if (
      expr["expression"].contains("typeName") &&
      expr["expression"]["typeName"].contains("name"))
      bs = expr["expression"]["typeName"]["name"].get<std::string>();
    else if (0)
    {
      //TODO：support something like <address>.balance
    }
    else
      return true;

    std::string mem = expr["memberName"].get<std::string>();
    std::string id_var = "c:@" + bs + "_" + mem;
    std::string id_func = "c:@F@" + bs + "_" + mem;
    if (context.find_symbol(id_var) != nullptr)
    {
      symbolt &sym = *context.find_symbol(id_var);

      if (sym.value.is_empty() || sym.value.is_zero())
      {
        // update: set the value to rand (default 0）
        // since all the current support built-in vars are uint type.
        // we just set the value to c:@F@nondet_uint
        symbolt &r = *context.find_symbol("c:@F@nondet_uint");
        sym.value = r.value;
      }
      new_expr = symbol_expr(sym);
    }

    else if (context.find_symbol(id_func) != nullptr)
      new_expr = symbol_expr(*context.find_symbol(id_func));
    else
      return true;
  }
  else
    return true;

  return false;
}

// the input type_name is objtype node
bool clarity_convertert::get_type_description(
  const nlohmann::json &type_name,
  typet &new_type)
{
  log_status(
    "Got type description {} {}", "get_type_description", type_name.dump());
  // For Clarity rule type-name:
  ClarityGrammar::TypeNameT type = ClarityGrammar::get_type_name_t(type_name);

  log_status(
    "Got current functionName {} {}", "get_type_description", int(type));
  switch (type)
  {
  case ClarityGrammar::TypeNameT::ElementaryTypeName:
  {
    // rule state-variable-declaration
    return get_elementary_type_name(type_name, new_type);
  }
  case ClarityGrammar::TypeNameT::ParameterList:
  {
    log_status(
      "Got current functionName {} {}",
      "get_type_description",
      "ParameterList");

    // rule parameter-list
    // Used for Clarity function parameter or return list
    return get_parameter_list(type_name, new_type);
  }
  case ClarityGrammar::TypeNameT::BuffTypeName:
  {
    //it's a buffer of bytes
    // buff is an array of bytes
    // std::string str_buff_size = type_name[2];
    // int bit_width = 8 * std::stoi(str_buff_size);
    // new_type = unsignedbv_typet(bit_width);

    if (get_elementary_type_name_buff(type_name, new_type))
      return true;

    break;
  }
  case ClarityGrammar::TypeNameT::ListTypeName:
  {
    // Deal with array with constant size, e.g., int a[2]; Similar to clang::Type::ConstantArray

    // list is an array of entry-type
    //it's a list of entry-type

    get_list_type(type_name, new_type);

    break;
  }

  case ClarityGrammar::TypeNameT::ContractTypeName:
  {
    // ToDo
    // FIXME
    // e.g. ContractName tmp = new ContractName(Args);

    std::string constructor_name = type_name["typeString"].get<std::string>();
    size_t pos = constructor_name.find(" ");
    std::string id = prefix + constructor_name.substr(pos + 1);

    if (context.find_symbol(id) == nullptr)
      return true;

    const symbolt &s = *context.find_symbol(id);
    new_type = s.type;

    break;
  }
  case ClarityGrammar::TypeNameT::OptionalTypeName:
  {
    nlohmann::json optional_type = ClarityGrammar::get_optional_type(type_name);
    std::string symbol_id = ClarityGrammar::get_optional_symbolId(
      optional_type); //"tag-struct optional_int128_t";
    // ClarityGrammar::ElementaryTypeNameT optional_typet = ClarityGrammar::get_elementary_type_name_t(optional_type);

    if (context.find_symbol(symbol_id) == nullptr)
    {
      log_error(
        "Optional struct {} not found in the symbol table. Aborting...",
        symbol_id);
      return true;
    }
    else
    {
      const symbolt &sym = *context.find_symbol(symbol_id);
      new_type = sym.type;
    }
    break;
  }
  case ClarityGrammar::TypeNameT::MappingTypeName:
  {
    // e.g.
    //  "typeIdentifier": "t_mapping$_t_uint256_$_t_string_storage_$",
    //  "typeString": "mapping(uint256 => string)"
    // since the key will always be regarded as string, we only need to obtain the value type.

    typet val_t;
    //!TODO Unimplement Mapping
    log_error("Unimplement Mapping");
    abort();
    break;
  }
  case ClarityGrammar::TypeNameT::TupleTypeName:
  {
    new_type = struct_typet();
    new_type.set("#cpp_type", "void");
    new_type.set("#clar_type", "tuple");
    break;
  }
  // ml-returntype is handled for a function in another mechanism.
  // case ClarityGrammar::TypeNameT::ReturnTypeName:
  // {
  //   log_status("Got current functionName {} {}","get_type_description", "ReturnTypeName");
  //   // For now lets create a struct with type return and handle it later
  //   // new_type = struct_typet();
  //   // new_type.set("#cpp_type", "void");
  //   // new_type.set("#clar_type", "return");
  //   if (get_type_description(ast_node[1]["objtype"], new_type))
  //     return true;
  //   break;
  // }
  default:
  {
    log_debug(
      "clarity",
      "	@@@ got type name=ClarityGrammar::TypeNameT::{}",
      ClarityGrammar::type_name_to_str(type));
    assert(!"Unimplemented type in rule type-name");
    return true;
  }
  }

  // TODO: More var decl attributes checks:
  //    - Constant
  //    - Volatile
  //    - isRestrict

  return false;
}

bool clarity_convertert::get_func_decl_ref_type(
  const nlohmann::json &decl,
  typet &new_type)
{
  // For FunctionToPointer decay:
  // Get type when we make a function call:
  //  - FunnctionNoProto: x = nondet()
  //  - FunctionProto:    z = add(x, y)
  // Similar to the function get_type_description()
  ClarityGrammar::FunctionDeclRefT type =
    ClarityGrammar::get_func_decl_ref_t(decl);

  switch (type)
  {
  case ClarityGrammar::FunctionDeclRefT::FunctionNoProto:
  {
    code_typet type;

    // Return type
    const nlohmann::json &rtn_type = decl["returnParameters"];

    typet return_type;
    if (get_type_description(rtn_type, return_type))
      return true;

    type.return_type() = return_type;

    if (!type.arguments().size())
      type.make_ellipsis();

    new_type = type;
    break;
  }
  case ClarityGrammar::FunctionDeclRefT::FunctionProto:
  {
    code_typet type;

    // store current state
    const nlohmann::json *old_functionDecl = current_functionDecl;
    const std::string old_functionName = current_functionName;

    // need in get_function_params()
    current_functionName = decl["name"].get<std::string>();
    current_functionDecl = &decl;

    const nlohmann::json &rtn_type = decl["returnParameters"];

    typet return_type;
    if (get_type_description(rtn_type, return_type))
      return true;

    type.return_type() = return_type;
    // convert parameters if the function has them
    // update the typet, since typet contains parameter annotations
    for (const auto &decl : decl["parameters"]["parameters"].items())
    {
      const nlohmann::json &func_param_decl = decl.value();

      code_typet::argumentt param;
      if (get_function_params(func_param_decl, param))
        return true;

      type.arguments().push_back(param);
    }

    current_functionName = old_functionName;
    current_functionDecl = old_functionDecl;

    new_type = type;
    break;
  }
  default:
  {
    log_debug(
      "clarity",
      "	@@@ Got type={}",
      ClarityGrammar::func_decl_ref_to_str(type));
    //TODO: seem to be unnecessary, need investigate
    // assert(!"Unimplemented type in auxiliary type to convert function call");
    return true;
  }
  }

  // TODO: More var decl attributes checks:
  //    - Constant
  //    - Volatile
  //    - isRestrict
  return false;
}

bool clarity_convertert::get_array_to_pointer_type(
  const nlohmann::json &type_descriptor,
  typet &new_type)
{
  // Function to get the base type in ArrayToPointer decay
  //  - unrolled the get_type...
  if (
    type_descriptor["typeString"].get<std::string>().find("uint8") !=
    std::string::npos)
  {
    new_type = unsigned_char_type();
    new_type.set("#cpp_type", "unsigned_char");
  }
  else
    assert(!"Unsupported types in ArrayToPinter decay");

  // TODO: More var decl attributes checks:
  //    - Constant
  //    - Volatile
  //    - isRestrict
  return false;
}

bool clarity_convertert::get_tuple_definition(const nlohmann::json &ast_node)
{
  struct_typet t = struct_typet();

  // get name/id:
  std::string name, id;
  get_tuple_name(ast_node, name, id);

  // get type:
  t.tag("struct " + name);

  // get location
  locationt location_begin;
  get_location_from_decl(ast_node[1], location_begin);

  // get debug module name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());
  current_fileName = debug_modulename;

  // populate struct type symbol
  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, t, name, id, location_begin);
  symbol.is_type = true;
  symbolt &added_symbol = *move_symbol_to_context(symbol);

  // populate params
  //TODO: flatten the nested tuple (e.g. ((x,y),z) = (func(),1); )
  size_t counter = 0;
  nlohmann::json objtype = ast_node[1]["objtype"][1];
  //std::cout <<objtype.dump(4)<<std::endl;

  //  //for loop to iterate over all keys of objtype
  // ml- this is an array. need to traverse the array
  //for (nlohmann::json::iterator it = objtype.begin(); it != objtype.end(); ++it)
  for (auto &it : objtype)
  {
    struct_typet::componentt comp;

    // manually create a member_name
    // follow the naming rule defined in get_var_decl_name
    assert(!current_contractName.empty());
    //const std::string mem_name = "mem" + std::to_string(counter);
    const std::string mem_name = it[0]; //it.key();
    const std::string mem_id = "clar:@C@" + current_contractName + "@" + name +
                               "@" + mem_name + "#" +
                               i2string(ast_node[1]["cid"].get<std::int16_t>());

    // get type
    typet mem_type;
    //if (get_type_description(it.value(), mem_type))
    if (get_type_description(it[1], mem_type))
      return true;

    // construct comp
    comp.type() = mem_type;
    comp.type().set("#member_name", t.tag());
    comp.identifier(mem_id);
    comp.cmt_lvalue(true);
    comp.name(mem_name);
    comp.pretty_name(mem_name);
    //comp.set_access("internal");

    // update struct type component
    t.components().push_back(comp);

    // update cnt
    ++counter;
  }

  t.location() = location_begin;
  added_symbol.type = t;
  added_symbol.is_type = true;

  return false;
}

// this function performs the pre-processing required to use a symbol defined in C template.
// takes input :
//  id -> prefilled to id of struct to look for in the symbol table
// ast_node -> the ast node containing the declaration info
// outputs:
// location as location_begin
// symbol as added_symbol
// struct type as t
// returns :
//  false for success
//  true for failure
bool clarity_convertert::process_c_defined_structs(
  std::string &id,
  const nlohmann::json &ast_node,
  locationt &location_begin,
  symbolt &added_symbol,
  exprt &inits,
  typet &t)
{
  std::string name;

  if (context.find_symbol(id) == nullptr)
  {
    log_error("Type {} not found in the symbol table. Aborting...", id);
    return true;
  }

  const symbolt &sym = *context.find_symbol(id);

  // get type
  t = sym.type;
  assert(t.id() == typet::id_struct);

  // get instance name,id
  get_state_var_decl_name(ast_node, name, id);

  // get location
  //locationt location_begin;
  get_location_from_decl(ast_node, location_begin);

  // get debug module name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());
  current_fileName = debug_modulename;

  // populate struct type symbol
  symbolt symbol;

  if (context.find_symbol(id) != nullptr)
  {
    //log_status("Symbol {} already exists in the context", id);
    symbol = *context.find_symbol(id);
  }
  else
  {
    // the symbol should already be in the space. this is an error if you don't find the symbol already defined

    return true;
  }

  //symbolt &added_symbol = symbol;
  added_symbol = symbol;

  inits = gen_zero(t);

  int is = inits.operands().size();
  int as = to_struct_type(t).components().size();
  assert(is <= as);

  return false;
}

bool clarity_convertert::get_list_of_entry_type(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  std::string id = get_list_struct_id(ast_node[1]["objtype"]);
  locationt location_begin;
  symbolt added_symbol;
  exprt inits;
  typet t;
  process_c_defined_structs(
    id, ast_node, location_begin, added_symbol, inits, t);

  size_t i = 0;

  for (auto &opds : to_struct_type(t).components())
  {
    struct_typet::componentt comp;

    // manually create a member_name
    std::string key = opds.name().as_string();
    std::string val_type =
      opds.type()
        .get("#cpp_type")
        .as_string(); //fixme : unreliable way of getting type
    std::string val_size = "1";
    nlohmann::json value_node = ast_node[1]["value"];

    if (key == "size")
    {
      // there is no need to translate size for list because there is no way to "get-size" of the list
      i++;
      continue;
    }

    if (val_type == "")
    {
      val_type = ast_node[1]["objtype"][3]["objtype"][0];
      val_size = ast_node[1]["objtype"][2];
    }

    const std::string mem_name = key;

    // get type
    typet subtype = opds.type();
    typet type = array_typet(
      subtype, from_integer(std::stoi(val_size), size_type())); //opds.type();
    // is array_typet(entryType as subtype, from_integer(size of list, size_type()))
    exprt buff_inits = gen_zero(type);

    nlohmann::json objtype = {val_type, val_type, val_size};

    int entry_indx = 0;
    int value_length = std::stoi(val_size);
    for (entry_indx = 0; entry_indx < value_length; entry_indx++)
    {
      nlohmann::json temp_expression_node;
      temp_expression_node["expressionType"] = "Literal";
      temp_expression_node["span"] = ast_node[1]["span"];
      temp_expression_node["identifier"] = mem_name;
      temp_expression_node["cid"] = ast_node[1]["cid"];
      temp_expression_node["objtype"] = objtype;
      temp_expression_node["value"] =
        ast_node[1]["value"]
                [entry_indx +
                 1]; //+1 because [0] index contains "list" identifier

      nlohmann::json temp_declarative_node = {"list", temp_expression_node};
      exprt init;
      if (get_expr(temp_declarative_node, objtype, init))
        return true;

      buff_inits.operands().at(entry_indx) = init;
    }

    const struct_typet::componentt *c = &to_struct_type(t).components().at(i);
    typet elem_type = c->type();

    clarity_gen_typecast(ns, buff_inits, elem_type);
    inits.operands().at(i) = buff_inits;

    // update
    ++i;
  }

  added_symbol.value = inits;
  new_expr = added_symbol.value;
  new_expr.identifier(id);

  return false;
}

bool clarity_convertert::get_optional_instance(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  nlohmann::json objtype =
    ClarityGrammar::get_optional_type(ast_node[1]["objtype"]);
  std::string id = ClarityGrammar::get_optional_symbolId(objtype);
  locationt location_begin;
  symbolt added_symbol;
  exprt inits;
  typet t;
  process_c_defined_structs(
    id, ast_node, location_begin, added_symbol, inits, t);

  size_t i = 0;

  //for (auto& [key, value]: optional_struct_members)
  for (auto &opds : (to_struct_type(t).components()))
  {
    struct_typet::componentt comp;

    // manually create a member_name
    std::string key = opds.name().as_string();
    std::string val_type =
      opds.type()
        .get("#cpp_type")
        .as_string(); //fixme : unreliable way of getting type
    std::string val_size = "1";

    // buff is missing for translation
    // it it looks like that should match the same methodology as for string-xxx types
    if (val_type == "")
    {
      val_type = ast_node[1]["objtype"][3][0];
      val_size = ast_node[1]["objtype"][3][2];
    }

    const std::string mem_name = key; //it.key();

    // get type
    typet mem_type;
    nlohmann::json objtype = {val_type, val_type, val_size};

    /* Create a temporary JSON object to ease processing */
    nlohmann::json temp_expression_node;
    temp_expression_node["expressionType"] =
      "Literal"; //fixme: gotta map this for non-literal values as well
    temp_expression_node["span"] = ast_node[1]["span"];
    temp_expression_node["identifier"] = mem_name;
    temp_expression_node["cid"] = ast_node[1]["cid"];
    temp_expression_node["objtype"] = objtype;

    // adjust value node accordingly.
    if (key == "is_none")
    {
      temp_expression_node["value"] =
        (ast_node[1]["value"][0] == "none") ? "true" : "false";
    }
    else if (key == "value")
    {
      // if optional was set as none, then we don't need to set any value for the value field.
      if (ast_node[1]["value"][0] == "none")
      {
        continue;
      }

      temp_expression_node["value"] = ast_node[1]["value"][1];
    }

    // the first argument of this declarative node is meaningless to the code that reads it.
    nlohmann::json temp_declarative_node = {"optional", temp_expression_node};

    //std::cout <<temp_declarative_node.dump(4)<<std::endl;

    exprt init;
    if (get_expr(temp_declarative_node, objtype, init))
      return true;

    const struct_typet::componentt *c = &to_struct_type(t).components().at(i);
    typet elem_type = c->type();

    clarity_gen_typecast(ns, init, elem_type);
    inits.operands().at(i) = init;

    // update
    ++i;
  }

  added_symbol.value = inits;
  new_expr = added_symbol.value;
  new_expr.identifier(id);
  return false;
}

bool clarity_convertert::get_principal_instance(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  std::string name, id;
  id = "tag-struct principal";

  locationt location_begin;
  symbolt added_symbol;
  exprt inits;
  typet t;
  process_c_defined_structs(
    id, ast_node, location_begin, added_symbol, inits, t);
  t.set("#clar_type", "principal_instance");

  size_t i = 0;

  //for (auto& [key, value]: principal_struct_members)
  for (auto &opds : to_struct_type(t).components())
  {
    struct_typet::componentt comp;
    std::string key = opds.name().as_string();

    std::string value_type =
      opds.type()
        .get("#cpp_type")
        .as_string(); //this is unreliable way to get the type of the member.
    std::string value_size = "1";
    std::string cformat_value =
      opds.type().find("size").find("#cformat").pretty();
    if (cformat_value == "nil")
    {
      value_size = "1";
    }
    else
    {
      value_size = cformat_value;
    }

    // manually create a member_name
    const std::string mem_name = key; //it.key();

    /* Create a temporary JSON object to ease processing */
    nlohmann::json temp_expression_node;

    // adjust value node accordingly.
    if (key == "contract_is_principal")
    {
      temp_expression_node["value"] = ClarityGrammar::is_standard_principal(ast_node);
        //(ast_node[1]["principalType"] == "contract" ? "true" : "false");
      value_type = "bool";
    }
    else if (key == "contract_is_standard")
    {
      temp_expression_node["value"] =
        (ast_node[1]["principalType"] == "standard" ? "true" : "false");
      value_type = "bool";
    }
    else if (key == "contract_name")
    {
      temp_expression_node["value"]["lit_ascii"] = ast_node[1]["contractName"];
      value_type = "string-ascii";
    }
    else if (key == "issuer_principal_bytes")
    {
      temp_expression_node["value"]["lit_utf8"] =
        ast_node[1]["issuerPrincipal"];
      value_type = "string-utf8";
    }
    else if (key == "version")
    {
      temp_expression_node["value"]["lit_utf8"] = "1";
      value_type = "string-utf8";
    }
    else if (key == "issuer_principal_str")
    {
      temp_expression_node["value"]["lit_ascii"] = "issuerPrincipalStr";
      value_type = "string-ascii";
    }

    // get type
    typet mem_type;
    nlohmann::json objtype = {
      value_type, value_type, value_size}; //{value[0],value[0],value[2]};

    temp_expression_node["expressionType"] = "Literal";
    temp_expression_node["span"] = ast_node[1]["span"];
    temp_expression_node["identifier"] = mem_name;
    temp_expression_node["cid"] = ast_node[1]["cid"];
    temp_expression_node["objtype"] = objtype;

    //std::cout <<temp_expression_node.dump(4)<<std::endl;

    nlohmann::json temp_declarative_node = {"principal", temp_expression_node};

    exprt init;
    if (get_expr(temp_declarative_node, objtype, init))
      return true;

    const struct_typet::componentt *c = &to_struct_type(t).components().at(i);
    typet elem_type = c->type();

    clarity_gen_typecast(ns, init, elem_type);
    inits.operands().at(i) = init;

    // update
    ++i;
  }

  added_symbol.value = inits;
  new_expr = added_symbol.value;
  new_expr.identifier(id);
  return false;
}

bool clarity_convertert::get_tuple_instance(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  std::string name, id;
  get_tuple_name(ast_node, name, id);

  if (context.find_symbol(id) == nullptr)
    return true;
  const symbolt &sym = *context.find_symbol(id);

  // get type
  typet t = sym.type;
  t.set("#clar_type", "tuple_instance");
  assert(t.id() == typet::id_struct);

  // get instance name,id
  if (get_tuple_instance_name(ast_node, name, id))
    return true;

  // get location
  locationt location_begin;
  get_location_from_decl(ast_node[1], location_begin);

  // get debug module name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());
  current_fileName = debug_modulename;

  // populate struct type symbol
  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, t, name, id, location_begin);

  symbol.lvalue = true;
  symbol.static_lifetime = true;
  symbol.file_local = false;
  symbol.is_extern = false;
  symbolt &added_symbol = *move_symbol_to_context(symbol);

  // populate initial value

  exprt inits = gen_zero(t);
  nlohmann::json objtype = ast_node[1]["objtype"];

  size_t i = 0;
  int is = inits.operands().size();
  int as = std::stoi(objtype[2].get<std::string>());
  assert(is <= as);

  // for (nlohmann::json::iterator it = objtype[1].begin(); it != objtype[1].end();
  //      ++it)
  nlohmann::json tupleElements = ast_node[1]["objtype"][1];
  //std::cout <<objtype.dump(4)<<std::endl;

  //  //for loop to iterate over all keys of objtype
  // ml- this is an array. need to traverse the array
  //for (nlohmann::json::iterator it = objtype.begin(); it != objtype.end(); ++it)
  for (auto &it : tupleElements)
  {
    //get the type of the component
    nlohmann::json component_type = it[1]; //it.value();
    std::string tuple_key = it[0];         //it.key();
    ClarityGrammar::TypeNameT type =
      ClarityGrammar::get_type_name_t(component_type);

    //Fixme: right now it only handles elementary types
    // we will need to pass it the whole get_expr function to handle more complex types.
    // recursive call to get_expr
    ClarityGrammar::ElementaryTypeNameT type_name =
      ClarityGrammar::get_elementary_type_name_t(component_type);

    /* Create a temporary JSON object to ease processing */
    nlohmann::json temp_expression_node;
    temp_expression_node["expressionType"] = "Literal";
    temp_expression_node["span"] = ast_node[1]["span"];
    temp_expression_node["identifier"] = tuple_key;
    temp_expression_node["cid"] = ast_node[1]["cid"];
    temp_expression_node["objtype"] = component_type;
    temp_expression_node["value"] = ast_node[1]["value"][1][tuple_key];

    nlohmann::json temp_declarative_node = {"tuple", temp_expression_node};

    //std::string the_value = expr[1][tuple_key].get<std::string>();
    //std::cout << temp_declarative_node.dump(4) << std::endl;
    exprt init;
    if (get_expr(temp_declarative_node, component_type, init))
      return true;

    const struct_typet::componentt *c = &to_struct_type(t).components().at(i);
    typet elem_type = c->type();

    clarity_gen_typecast(ns, init, elem_type);
    inits.operands().at(i) = init;

    // update
    ++i;
  }

  added_symbol.value = inits;
  new_expr = added_symbol.value;
  new_expr.identifier(id);
  return false;
}

void clarity_convertert::get_tuple_name(
  const nlohmann::json &ast_node,
  std::string &name,
  std::string &id)
{
  name =
    "tuple_" +
    ast_node[1]["identifier"]
      .get<
        std::string>(); //+ "#" + std::to_string(ast_node[1]["cid"].get<int>());
  id = prefix + "struct " + name;
}

bool clarity_convertert::get_tuple_instance_name(
  const nlohmann::json &ast_node,
  std::string &name,
  std::string &id)
{
  std::string c_name;
  if (get_current_contract_name(ast_node, c_name))
    return true;
  if (c_name.empty())
    return true;

  //name = "tuple_instance_" + ast_node[1]["identifier"].get<std::string>(); //+ "#"+ std::to_string(ast_node[1]["cid"].get<int>());
  name =
    ast_node[1]["identifier"]
      .get<
        std::string>(); //+ "#"+ std::to_string(ast_node[1]["cid"].get<int>());
  id = "clar:@C@" + c_name + "@" + name;
  return false;
}

/*
  obtain the corresponding tuple struct instance from the symbol table
  based on the function definition json
*/
bool clarity_convertert::get_tuple_function_ref(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  assert(ast_node.contains("nodeType") && ast_node["nodeType"] == "Identifier");

  std::string c_name;
  if (get_current_contract_name(ast_node, c_name))
    return true;
  if (c_name.empty())
    return true;

  std::string name =
    "tuple_instance" +
    std::to_string(ast_node["referencedDeclaration"].get<int>());
  std::string id = "clar:@C@" + c_name + "@" + name;

  if (context.find_symbol(id) == nullptr)
    return true;

  new_expr = symbol_expr(*context.find_symbol(id));
  return false;
}

// Knowing that there is a component x in the struct_tuple A, we construct A.x
bool clarity_convertert::get_tuple_member_call(
  const irep_idt instance_id,
  const exprt &comp,
  exprt &new_expr)
{
  // tuple_instance
  assert(!instance_id.empty());
  exprt base;
  if (context.find_symbol(instance_id) == nullptr)
    return true;

  base = symbol_expr(*context.find_symbol(instance_id));
  new_expr = member_exprt(base, comp.name(), comp.type());
  return false;
}

void clarity_convertert::get_tuple_function_call(
  code_blockt &_block,
  const exprt &op)
{
  assert(op.id() == "sideeffect");
  exprt func_call = op;
  convert_expression_to_code(func_call);
  _block.move_to_operands(func_call);
}

void clarity_convertert::get_tuple_assignment(
  code_blockt &_block,
  const exprt &lop,
  exprt rop)
{
  exprt assign_expr = side_effect_exprt("assign", lop.type());
  assign_expr.copy_to_operands(lop, rop);

  convert_expression_to_code(assign_expr);
  _block.move_to_operands(assign_expr);
}

/**
     * @brief Populate the out `typet` parameter with the uint type specified by type parameter
     *
     * @param type The type of the uint to be poulated
     * @param out The variable that holds the resulting type
     * @return true iff population failed
     * @return false iff population was successful
     */
bool clarity_convertert::get_elementary_type_name_uint(
  ClarityGrammar::ElementaryTypeNameT &type,
  typet &out)
{
  const unsigned int uint_size = ClarityGrammar::uint_type_name_to_size(type);
  out = unsignedbv_typet(uint_size);

  return false;
}

/**
     * @brief Populate the out `typet` parameter with the int type specified by type parameter
     *
     * @param type The type of the int to be poulated
     * @param out The variable that holds the resulting type
     * @return false iff population was successful
     */
bool clarity_convertert::get_elementary_type_name_int(
  ClarityGrammar::ElementaryTypeNameT &type,
  typet &out)
{
  const unsigned int int_size = ClarityGrammar::int_type_name_to_size(type);
  out = signedbv_typet(int_size);

  return false;
}

// get the id of the struct that represents the list
// takes parent objtype as input
// Parent_objtype contains ["list", "list", size_of_list, {"objtype": [entry_type, entry_type, size per entry]}]
std::string
clarity_convertert::get_list_struct_id(const nlohmann::json &objtype)
{
  // get the id of the struct that represents the list
  // e.g. list_uint128_t
  return "tag-struct " + objtype[0].get<std::string>() + "_" +
         objtype[3]["objtype"][0].get<std::string>();
}

bool clarity_convertert::get_list_type(
  const nlohmann::json &parent_objtype,
  typet &out)
{
  // For Clarity rule entry-type-list:

  // retrieve relevant list_type

  // e-g list_bool , list_uint128_t etc

  // parent_objtype contains ["list", "list", size_of_list, {"objtype": [entry_type, entry_type, size per entry]}]

  //nlohmann::json child_objtype = parent_objtype[3]["objtype"];

  // get types
  std::string id = get_list_struct_id(
    parent_objtype); //"tag-struct " + parent_objtype[0].get<std::string>() + "_" + child_objtype[0].get<std::string>();
  if (context.find_symbol(id) == nullptr)
  {
    log_error("Type {} not found in the symbol table. Aborting...", id);
    return true;
  }

  const symbolt &sym = *context.find_symbol(id);
  out = sym.type;

  return false;
  // is array_typet(entryType as subtype, from_integer(size of list, size_type()))
}

bool clarity_convertert::get_elementary_type_name_buff(
  const nlohmann::json &objtype,
  typet &out)
{
  /*
    bytes1 has size of 8 bits (possible values 0x00 to 0xff),
    which you can implicitly convert to uint8 (unsigned integer of size 8 bits) but not to int8
  */

  std::string str_buff_size = objtype[2];
  const unsigned int bytes = std::stoi(str_buff_size);
  out = array_typet(unsigned_char_type(), from_integer(bytes, size_type()));
  out.set("#clar_lit_type", "BUFF");
  return false;
}

bool clarity_convertert::get_elementary_type_name_bytesn(
  const nlohmann::json &objtype,
  typet &out)
{
  /*
    bytes1 has size of 8 bits (possible values 0x00 to 0xff),
    which you can implicitly convert to uint8 (unsigned integer of size 8 bits) but not to int8
  */
  std::string str_buff_size = objtype[2];
  const unsigned int bytes = std::stoi(str_buff_size);
  out = unsignedbv_typet(bytes * 8);

  return false;
}

// input param type_name is objtype node
bool clarity_convertert::get_elementary_type_name(
  const nlohmann::json &objtype,
  typet &new_type)
{
  // For Clarity rule elementary-type-name:
  // equivalent to clang's get_builtin_type()
  std::string c_type;
  ClarityGrammar::ElementaryTypeNameT type =
    ClarityGrammar::get_elementary_type_name_t(objtype);

  log_status(
    "clarity"
    "	@@@ got ElementaryType: ClarityGrammar::ElementaryTypeNameT::{}",
    int(type));

  switch (type)
  {
  // rule unsigned-integer-type
  case ClarityGrammar::ElementaryTypeNameT::UINT_LITERAL:
  case ClarityGrammar::ElementaryTypeNameT::UINT:
  {
    if (get_elementary_type_name_uint(type, new_type))
      return true;
    break;
  }
  case ClarityGrammar::ElementaryTypeNameT::INT_LITERAL:
  case ClarityGrammar::ElementaryTypeNameT::INT:
  {
    if (get_elementary_type_name_int(type, new_type))
      return true;
    break;
  }

  case ClarityGrammar::ElementaryTypeNameT::BOOL:
  {
    // ml-[TODO] improve this
    new_type = bool_type();
    c_type = "bool";
    new_type.set("#cpp_type", c_type);
    break;
  }
  case ClarityGrammar::ElementaryTypeNameT::STRING_ASCII:
  {
    // obj[2] contains size of object
    std::string str_value_length = objtype[2];
    size_t value_length = std::stoi(str_value_length);

    new_type = array_typet(
      signed_char_type(),
      constant_exprt(
        integer2binary(value_length, bv_width(int_type())),
        integer2string(value_length),
        int_type()));
    new_type.set("#clar_lit_type", "STRING_ASCII");
    break;
  }
  case ClarityGrammar::ElementaryTypeNameT::STRING_UTF8:
  {
    // TODO: and this
    std::string str_value_length = objtype[2];
    size_t value_length = std::stoi(str_value_length);

    new_type = array_typet(
      signed_char_type(),
      constant_exprt(
        integer2binary(value_length, bv_width(int_type())),
        integer2string(value_length),
        int_type()));
    new_type.set("#clar_lit_type", "STRING_UTF8");
    break;
  }
  case ClarityGrammar::ElementaryTypeNameT::PRINCIPAL:
  {
    std::string symbol_id = "tag-struct principal";
    if (context.find_symbol(symbol_id) == nullptr)
    {
      log_error(
        "Principal struct {} not found in the symbol table. Aborting...",
        symbol_id);
      return true;
    }
    else
    {
      const symbolt &sym = *context.find_symbol(symbol_id);
      new_type = sym.type;
    }
    break;
  }
  case ClarityGrammar::ElementaryTypeNameT::BUFF:
  {
    if (get_elementary_type_name_buff(objtype, new_type))
      return true;

    // for type conversion
    //new_type.set("#clar_type", elementary_type_name_to_str(type));
    //new_type.set("#clar_bytes_size", bytesn_type_name_to_size(type));

    break;
  }
  default:
  {
    log_debug(
      "clarity",
      "	@@@ Got elementary-type-name={}",
      ClarityGrammar::elementary_type_name_to_str(type));
    assert(!"Unimplemented type in rule elementary-type-name");
    return true;
  }
  }

  return false;
}

bool clarity_convertert::get_parameter_list(
  const nlohmann::json &type_name,
  typet &new_type)
{
  // For Clarity rule parameter-list:
  //  - For non-empty param list, it may need to call get_elementary_type_name, since parameter-list is just a list of types
  std::string c_type;
  ClarityGrammar::ParameterListT type =
    ClarityGrammar::get_parameter_list_t(type_name[3]);

  switch (type)
  {
  case ClarityGrammar::ParameterListT::EMPTY:
  {
    // equivalent to clang's "void"
    new_type = empty_typet();
    c_type = "void";
    new_type.set("#cpp_type", c_type);
    break;
  }
  case ClarityGrammar::ParameterListT::ONE_PARAM:
  {
    assert(
      type_name["parameters"].size() ==
      1); // TODO: Fix me! assuming one return parameter
    const nlohmann::json &rtn_type =
      type_name["parameters"].at(0)["typeDescriptions"];
    return get_type_description(rtn_type, new_type);

    break;
  }
  case ClarityGrammar::ParameterListT::MORE_THAN_ONE_PARAM:
  {
    // if contains multiple return types
    // We will return null because we create the symbols of the struct accordingly
    assert(type_name["parameters"].size() > 1);
    new_type = empty_typet();
    new_type.set("#cpp_type", "void");
    new_type.set("#clar_type", "tuple");
    break;
  }
  default:
  {
    assert(!"Unimplemented type in rule parameter-list");
    return true;
  }
  }

  return false;
}

// parse the state variable
void clarity_convertert::get_state_var_decl_name(
  const nlohmann::json &ast_node,
  std::string &name,
  std::string &id)
{
  // Follow the way in clang:
  //  - For state variable name, just use the ast_node["name"]
  //  - For state variable id, add prefix "clar:@"
  std::string contract_name;
  if (get_current_contract_name(ast_node, contract_name))
  {
    log_error("Internal error when obtaining the contract name. Aborting...");
    abort();
  }
  name =
    ast_node["identifier"]
      .get<
        std::
          string>(); // assume Clarity AST json object has "identifier" field, otherwise throws an exception in nlohmann::json

  // e.g. clar:@C@Base@x#11
  // The prefix is used to avoid duplicate names
  if (!contract_name.empty())
  {
    if ((ast_node.contains("objtype") &&
         (ClarityGrammar::is_tuple_declaration(ast_node))))
    {
      get_tuple_name(ast_node, name, id);
    }
    else
    {
      id = "clar:@C@" + contract_name + "@" + name + "#" +
           i2string(ast_node["cid"].get<std::int16_t>());
    }
  }
  else
    id = "clar:@" + name + "#" + i2string(ast_node["cid"].get<std::int16_t>());
}

// parse the non-state variable
void clarity_convertert::get_var_decl_name(
  const nlohmann::json &ast_node,
  std::string &name,
  std::string &id)
{
  std::string contract_name;
  if (get_current_contract_name(ast_node, contract_name))
  {
    log_error("Internal error when obtaining the contract name. Aborting...");
    abort();
  }

  name =
    ast_node["identifier"]
      .get<
        std::
          string>(); // assume Clarity AST json object has "name" field, otherwise throws an exception in nlohmann::json

  assert(ast_node.contains("cid"));
  if (
    current_functionDecl && !contract_name.empty() &&
    !current_functionName.empty())
  {
    // converting local variable inside a function
    // For non-state functions, we give it different id.
    // E.g. for local variable i in function nondet(), it's "clar:@C@Base@F@nondet@i#55".

    // As the local variable inside the function will not be inherited, we can use current_functionName
    id = "clar:@C@" + contract_name + "@F@" + current_functionName + "@" +
         name + "#" + i2string(ast_node["cid"].get<std::int16_t>());
  }
  else if (ast_node.contains("scope"))
  {
    // This means we are handling a local variable which is not inside a function body.
    //! Assume it is a variable inside struct/error
    int scp = ast_node["scope"].get<int>();
    if (scope_map.count(scp) == 0)
    {
      log_error("cannot find struct/error name");
      abort();
    }
    std::string struct_name = scope_map.at(scp);
    if (contract_name.empty())
      id = "clar:@" + struct_name + "@" + name + "#" +
           i2string(ast_node["id"].get<std::int16_t>());
    else
      id = "clar:@C@" + contract_name + "@" + struct_name + "@" + name + "#" +
           i2string(ast_node["id"].get<std::int16_t>());
  }
  else
  {
    log_error("Unsupported local variable");
    abort();
  }
}

void clarity_convertert::get_function_definition_name(
  const nlohmann::json &ast_node,
  std::string &name,
  std::string &id)
{
  // Follow the way in clang:
  //  - For function name, just use the ast_node["name"]
  // assume Clarity AST json object has "name" field, otherwise throws an exception in nlohmann::json
  std::string contract_name;
  assert(ast_node.contains("identifier"));
  if (get_current_contract_name(ast_node, contract_name))
  {
    log_error("Internal error when obtaining the contract name. Aborting...");
    abort();
  }

#if 0
  // ml-no such thing as constructor for now
  if (ast_node["kind"].get<std::string>() == "constructor")
  {
    name = contract_name;
    // constructors cannot be overridden, primarily because they don't have names
    // to align with the implicit constructor, we do not add the 'id'
    id = "clar:@C@" + contract_name + "@F@" + name + "#";
  }
  else
#endif
  {
    name = ast_node["identifier"].get<std::string>();
    id = "clar:@C@" + contract_name + "@F@" + name + "#" +
         i2string(ast_node["cid"].get<std::int16_t>());
  }
}

unsigned int clarity_convertert::add_offset(
  const std::string &src,
  unsigned int start_position)
{
  // extract the length from "start:length:index"
  std::string offset = src.substr(1, src.find(":"));
  // already added 1 in start_position
  unsigned int end_position = start_position + std::stoul(offset);
  return end_position;
}

std::string
clarity_convertert::get_ctor_call_id(const std::string &contract_name)
{
  return "clar:@C@" + contract_name + "@F@" + contract_name + "#";
}

std::string
clarity_convertert::get_src_from_json(const nlohmann::json &ast_node)
{
  // some nodes may have "src" inside a member json object
  // we need to deal with them case by case based on the node type
  ClarityGrammar::ExpressionT type = ClarityGrammar::get_expression_t(ast_node);
  switch (type)
  {
  case ClarityGrammar::ExpressionT::ImplicitCastExprClass:
  {
    assert(ast_node.contains("subExpr"));
    assert(ast_node["subExpr"].contains("src"));
    return ast_node["subExpr"]["src"].get<std::string>();
  }
  case ClarityGrammar::ExpressionT::NullExpr:
  {
    // empty address
    return "-1:-1:-1";
  }
  default:
  {
    assert(!"Unsupported node type when getting src from JSON");
    abort();
  }
  }
}

unsigned int clarity_convertert::get_line_number(
  const nlohmann::json &ast_node,
  bool final_position)
{
  /*
 [
      "data-var",
      {
        "identifier": "barf",
        "id": 3,
        "span": {
          "start_line": 3,
          "start_column": 18,
          "end_line": 3,
          "end_column": 21
        },
        "objtype": [
          "string-ascii",
          "16"
        ],
        "value": "abc"
      }
    ]
    */
  unsigned int loc = -1;
  if (ast_node.is_array())
  {
    ast_node[1]["span"]["start_line"].get<int>();
  }
  else if (ast_node.is_object())
  {
    ast_node["span"]["start_line"].get<int>();
  }
  return loc;
}

void clarity_convertert::get_location_from_decl(
  const nlohmann::json &ast_node,
  locationt &location)
{
  location.set_line(get_line_number(ast_node));
  location.set_file(
    absolute_path); // assume absolute_path is the name of the contrace file, since we ran solc in the same directory

  // To annotate local declaration within a function
  if (
    ClarityGrammar::is_variable_declaration(ast_node) &&
    ClarityGrammar::is_state_variable(ast_node) == false)
  {
    assert(
      current_functionDecl); // must have a valid current function declaration
    location.set_function(
      current_functionName); // set the function where this local variable belongs to
  }
}

void clarity_convertert::get_start_location_from_stmt(
  const nlohmann::json &ast_node,
  locationt &location)
{
  std::string function_name;

  if (current_functionDecl)
    function_name = current_functionName;

  // The src manager of Clarity AST JSON is too encryptic.
  // For the time being we are setting it to "1".

  // ml- if we are working on function expressions then they do not have the line numbers in the
  // statements. so use the function decleration and its line number
  if (!function_name.empty())
    location.set_line(get_line_number(*current_functionDecl));
  else
    location.set_line(get_line_number(ast_node));
  location.set_file(
    absolute_path); // assume absolute_path is the name of the contrace file, since we ran solc in the same directory

  if (!function_name.empty())
    location.set_function(function_name);
}

void clarity_convertert::get_final_location_from_stmt(
  const nlohmann::json &ast_node,
  locationt &location)
{
  std::string function_name;

  if (current_functionDecl)
    function_name = current_functionName;

  // The src manager of Clarity AST JSON is too encryptic.
  // For the time being we are setting it to "1".
  location.set_line(get_line_number(ast_node, true));
  location.set_file(
    absolute_path); // assume absolute_path is the name of the contrace file, since we ran solc in the same directory

  if (!function_name.empty())
    location.set_function(function_name);
}

std::string clarity_convertert::get_modulename_from_path(std::string path)
{
  std::string filename = get_filename_from_path(path);

  if (filename.find_last_of('.') != std::string::npos)
    return filename.substr(0, filename.find_last_of('.'));

  return filename;
}

std::string clarity_convertert::get_filename_from_path(std::string path)
{
  if (path.find_last_of('/') != std::string::npos)
    return path.substr(path.find_last_of('/') + 1);

  return path; // for _x, it just returns "overflow_2.c" because the test program is in the same dir as esbmc binary
}

// A wrapper to obtain additional contract_name info
const nlohmann::json &clarity_convertert::find_decl_ref(int ref_decl_id)
{
  // An empty contract name means that the node outside any contracts.
  std::string empty_contract_name = "";
  return find_decl_ref(ref_decl_id, empty_contract_name);
}

const nlohmann::json &
clarity_convertert::find_decl_ref(int ref_decl_id, std::string &contract_name)
{
  //TODO: Clean up this funciton. Such a mess...

  if (ref_decl_id < 0)
  {
    log_warning("Cannot find declaration reference for the built-in function.");
    abort();
  }

  // First, search state variable nodes
  nlohmann::json &nodes = src_ast_json["nodes"];
  unsigned index = 0;
  for (nlohmann::json::iterator itr = nodes.begin(); itr != nodes.end();
       ++itr, ++index)
  {
    // check the nodes outside of the contract
    // it can be referred to the data structure
    // or the members inside the structure.
    if ((*itr)["id"] == ref_decl_id)
      return nodes.at(index);

    if (
      (*itr)["nodeType"] == "EnumDefinition" ||
      (*itr)["nodeType"] == "StructDefinition")
    {
      unsigned men_idx = 0;
      nlohmann::json &mem_nodes = nodes.at(index)["members"];
      for (nlohmann::json::iterator mem_itr = mem_nodes.begin();
           mem_itr != mem_nodes.end();
           ++mem_itr, ++men_idx)
      {
        if ((*mem_itr)["id"] == ref_decl_id)
          return mem_nodes.at(men_idx);
      }
    }

    if ((*itr)["nodeType"] == "ErrorDefinition")
    {
      if (
        (*itr).contains("parameters") &&
        ((*itr)["parameters"]).contains("parameters"))
      {
        unsigned men_idx = 0;
        nlohmann::json &mem_nodes = (*itr)["parameters"]["parameters"];
        for (nlohmann::json::iterator mem_itr = mem_nodes.begin();
             mem_itr != mem_nodes.end();
             ++mem_itr, ++men_idx)
        {
          if ((*mem_itr)["id"] == ref_decl_id)
            return mem_nodes.at(men_idx);
        }
      }
    }

    // check the nodes inside a contract
    if ((*itr)["nodeType"] == "ContractDefinition")
    {
      // update the contract name first
      contract_name = (*itr)["name"].get<std::string>();

      nlohmann::json &ast_nodes = nodes.at(index)["nodes"];
      unsigned idx = 0;
      for (nlohmann::json::iterator itrr = ast_nodes.begin();
           itrr != ast_nodes.end();
           ++itrr, ++idx)
      {
        if ((*itrr)["id"] == ref_decl_id)
          return ast_nodes.at(idx);

        if (
          (*itrr)["nodeType"] == "EnumDefinition" ||
          (*itrr)["nodeType"] == "StructDefinition")
        {
          unsigned men_idx = 0;
          nlohmann::json &mem_nodes = ast_nodes.at(idx)["members"];

          for (nlohmann::json::iterator mem_itr = mem_nodes.begin();
               mem_itr != mem_nodes.end();
               ++mem_itr, ++men_idx)
          {
            if ((*mem_itr)["id"] == ref_decl_id)
              return mem_nodes.at(men_idx);
          }
        }

        if ((*itr)["nodeType"] == "ErrorDefinition")
        {
          if (
            (*itr).contains("parameters") &&
            ((*itr)["parameters"]).contains("parameters"))
          {
            unsigned men_idx = 0;
            nlohmann::json &mem_nodes = (*itr)["parameters"]["parameters"];
            for (nlohmann::json::iterator mem_itr = mem_nodes.begin();
                 mem_itr != mem_nodes.end();
                 ++mem_itr, ++men_idx)
            {
              if ((*mem_itr)["id"] == ref_decl_id)
                return mem_nodes.at(men_idx);
            }
          }
        }
      }
    }
  }

  //! otherwise, assume it is current_contractName
  contract_name = current_contractName;

  if (current_functionDecl != nullptr)
  {
    // Then search "declarations" in current function scope
    const nlohmann::json &current_func = *current_functionDecl;
    if (!current_func.contains("body"))
    {
      log_error(
        "Unable to find the corresponding local variable decl. Current "
        "function "
        "does not have a function body.");
      abort();
    }

    // var declaration in local statements
    // bfs visit
    std::queue<const nlohmann::json *> body_stmts;
    body_stmts.emplace(&current_func["body"]);

    while (!body_stmts.empty())
    {
      const nlohmann::json &top_stmt = *(body_stmts).front();
      for (const auto &body_stmt : top_stmt["statements"].items())
      {
        const nlohmann::json &stmt = body_stmt.value();
        if (stmt["nodeType"] == "VariableDeclarationStatement")
        {
          for (const auto &local_decl : stmt["declarations"].items())
          {
            const nlohmann::json &the_decl = local_decl.value();
            if (the_decl["id"] == ref_decl_id)
            {
              assert(the_decl.contains("nodeType"));
              return the_decl;
            }
          }
        }

        // nested block e.g.
        // {
        //    {
        //        int x = 1;
        //    }
        // }
        if (stmt["nodeType"] == "Block" && stmt.contains("statements"))
          body_stmts.emplace(&stmt);
      }

      body_stmts.pop();
    }

    // Search function parameter
    if (current_func.contains("parameters"))
    {
      if (current_func["parameters"]["parameters"].size())
      {
        // var decl in function parameter array
        for (const auto &param_decl :
             current_func["parameters"]["parameters"].items())
        {
          const nlohmann::json &param = param_decl.value();
          assert(param["nodeType"] == "VariableDeclaration");
          if (param["id"] == ref_decl_id)
            return param;
        }
      }
    }
  }

  // if no matching state or local var decl, search decl in current_forStmt
  if (current_forStmt != nullptr)
  {
    const nlohmann::json &current_for = *current_forStmt;
    if (current_for.contains("initializationExpression"))
    {
      if (current_for["initializationExpression"].contains("declarations"))
      {
        assert(current_for["initializationExpression"]["declarations"]
                 .size()); // Assuming a non-empty declaration array
        const nlohmann::json &decls =
          current_for["initializationExpression"]["declarations"];
        for (const auto &init_decl : decls.items())
        {
          const nlohmann::json &the_decl = init_decl.value();
          if (the_decl["id"] == ref_decl_id)
            return the_decl;
        }
      }
      else
        assert(!"Unable to find the corresponding local variable decl. No local declarations found in current For-Statement");
    }
    else
      assert(!"Unable to find the corresponding local variable decl. Current For-Statement does not have any init.");
  }

  // Instead of reporting errors here, we return an empty json
  // and leave the error handling to the caller.
  return empty_json;
}

// return construcor node
const nlohmann::json &clarity_convertert::find_constructor_ref(int ref_decl_id)
{
  nlohmann::json &nodes = src_ast_json["nodes"];
  unsigned index = 0;
  for (nlohmann::json::iterator itr = nodes.begin(); itr != nodes.end();
       ++itr, ++index)
  {
    if (
      (*itr)["id"].get<int>() == ref_decl_id &&
      (*itr)["nodeType"] == "ContractDefinition")
    {
      nlohmann::json &ast_nodes = nodes.at(index)["nodes"];
      unsigned idx = 0;
      for (nlohmann::json::iterator ittr = ast_nodes.begin();
           ittr != ast_nodes.end();
           ++ittr, ++idx)
      {
        if ((*ittr)["kind"] == "constructor")
        {
          return ast_nodes.at(idx);
        }
      }
    }
  }

  // implicit constructor call
  return empty_json;
}

void clarity_convertert::get_default_symbol(
  symbolt &symbol,
  std::string module_name,
  typet type,
  std::string name,
  std::string id,
  locationt location)
{
  symbol.mode = mode;
  symbol.module = module_name;
  symbol.location = std::move(location);
  symbol.type = std::move(type);
  symbol.name = name;
  symbol.id = id;
}

symbolt *clarity_convertert::move_symbol_to_context(symbolt &symbol)
{
  return context.move_symbol_to_context(symbol);
}

void clarity_convertert::convert_expression_to_code(exprt &expr)
{
  if (expr.is_code())
    return;

  codet code("expression");
  code.location() = expr.location();
  code.move_to_operands(expr);

  expr.swap(code);
}

bool clarity_convertert::check_intrinsic_function(
  const nlohmann::json &ast_node)
{
  // function to detect special intrinsic functions, e.g. __ESBMC_assume
  return (
    ast_node.contains("name") && (ast_node["name"] == "__ESBMC_assume" ||
                                  ast_node["name"] == "__VERIFIER_assume" ||
                                  ast_node["name"] == "__ESBMC_assert" ||
                                  ast_node["name"] == "__VERIFIER_assert"));
}

nlohmann::json clarity_convertert::make_implicit_cast_expr(
  const nlohmann::json &sub_expr,
  std::string cast_type)
{
  // Since Clarity AST does not have type cast information about return values,
  // we need to manually make a JSON object and wrap the return expression in it.
  std::map<std::string, std::string> m = {
    {"type", "ImplicitCastExprClass"},
    {"castType", cast_type},
    {"subExpr", {}}};
  nlohmann::json implicit_cast_expr = m;
  implicit_cast_expr["subExpr"] = sub_expr;

  return implicit_cast_expr;
}

nlohmann::json
clarity_convertert::make_pointee_type(const nlohmann::json &sub_expr)
{
  // Since Clarity function call node does not have enough information, we need to make a JSON object
  // manually create a JSON object to complete the conversions of function to pointer decay

  // make a mapping for JSON object creation latter
  // based on the usage of get_func_decl_ref_t() in get_func_decl_ref_type()
  nlohmann::json adjusted_expr;

  if (
    sub_expr["typeString"].get<std::string>().find("function") !=
    std::string::npos)
  {
    // Add more special functions here
    if (
      sub_expr["typeString"].get<std::string>().find("function ()") !=
        std::string::npos ||
      sub_expr["typeIdentifier"].get<std::string>().find(
        "t_function_assert_pure$") != std::string::npos ||
      sub_expr["typeIdentifier"].get<std::string>().find(
        "t_function_internal_pure$") != std::string::npos)
    {
      // e.g. FunctionNoProto: "typeString": "function () returns (uint8)" with () empty after keyword 'function'
      // "function ()" contains the function args in the parentheses.
      // make a type to behave like ClarityGrammar::FunctionDeclRefT::FunctionNoProto
      // Note that when calling "assert(.)", it's like "typeIdentifier": "t_function_assert_pure$......",
      //  it's also treated as "FunctionNoProto".
      auto j2 = R"(
            {
              "nodeType": "FunctionDefinition",
              "parameters":
                {
                  "parameters" : []
                }
            }
          )"_json;
      adjusted_expr = j2;

      if (
        sub_expr["typeString"].get<std::string>().find("returns") !=
        std::string::npos)
      {
        adjusted_expr = R"(
            {
              "nodeType": "FunctionDefinition",
              "parameters":
                {
                  "parameters" : []
                }
            }
          )"_json;
        // e.g. for typeString like:
        // "typeString": "function () returns (uint8)"
        // use regex to capture the type and convert it to shorter form.
        std::smatch matches;
        std::regex e("returns \\((\\w+)\\)");
        std::string typeString = sub_expr["typeString"].get<std::string>();
        if (std::regex_search(typeString, matches, e))
        {
          auto j2 = nlohmann::json::parse(
            R"({
                "typeIdentifier": "t_)" +
            matches[1].str() + R"(",
                "typeString": ")" +
            matches[1].str() + R"("
              })");
          adjusted_expr["returnParameters"]["parameters"][0]
                       ["typeDescriptions"] = j2;
        }
        else if (
          sub_expr["typeString"].get<std::string>().find("returns (contract") !=
          std::string::npos)
        {
          // TODO: Fix me
          auto j2 = R"(
              {
                "nodeType": "ParameterList",
                "parameters": []
              }
            )"_json;
          adjusted_expr["returnParameters"] = j2;
        }
        else
          assert(!"Unsupported return types in pointee");
      }
      else
      {
        // e.g. for typeString like:
        // "typeString": "function (bool) pure"
        auto j2 = R"(
              {
                "nodeType": "ParameterList",
                "parameters": []
              }
            )"_json;
        adjusted_expr["returnParameters"] = j2;
      }
    }
    else
      assert(!"Unsupported - detected function call with parameters");
  }
  else
    assert(!"Unsupported pointee - currently we only support the semantics of function to pointer decay");

  return adjusted_expr;
}

// Parse typet object into a typeDescriptions json
nlohmann::json clarity_convertert::make_return_type_from_typet(typet type)
{
  // Useful to get the width of a int literal type for return statement
  nlohmann::json adjusted_expr;
  if (type.is_signedbv() || type.is_unsignedbv())
  {
    std::string width = type.width().as_string();
    std::string type_name = (type.is_signedbv() ? "int" : "uint") + width;
    auto j2 = nlohmann::json::parse(
      R"({
              "typeIdentifier": "t_)" +
      type_name + R"(",
              "typeString": ")" +
      type_name + R"("
            })");
    adjusted_expr = j2;
  }
  return adjusted_expr;
}

nlohmann::json clarity_convertert::make_array_elementary_type(
  const nlohmann::json &type_descrpt)
{
  // Function used to extract the type of the array and its elements
  // In order to keep the consistency and maximum the reuse of get_type_description function,
  // we used ["typeDescriptions"] instead of ["typeName"], despite the fact that the latter contains more information.
  // Although ["typeDescriptions"] also contains all the information needed, we have to do some pre-processing

  // e.g.
  //   "typeDescriptions": {
  //     "typeIdentifier": "t_array$_t_uint256_$dyn_memory_ptr",
  //     "typeString": "uint256[] memory"
  //      }
  //
  // convert to
  //
  //   "typeDescriptions": {
  //     "typeIdentifier": "t_uint256",
  //     "typeString": "uint256"
  //     }

  //! current implement does not consider Multi-Dimensional Arrays

  // 1. declare an empty json node
  nlohmann::json elementary_type;
  const std::string typeIdentifier =
    type_descrpt["typeIdentifier"].get<std::string>();

  // 2. extract type info
  // e.g.
  //  bytes[] memory x => "t_array$_t_bytes_$dyn_storage_ptr"  => t_bytes
  //  [1,2,3]          => "t_array$_t_uint8_$3_memory_ptr      => t_uint8
  assert(typeIdentifier.substr(0, 8) == "t_array$");
  std::regex rgx("\\$_\\w*_\\$");

  std::smatch match;
  if (!std::regex_search(typeIdentifier, match, rgx))
    assert(!"Cannot find array element type in typeIdentifier");
  std::string sub_match = match[0];
  std::string t_type = sub_match.substr(2, sub_match.length() - 4);
  std::string type = t_type.substr(2);

  // 3. populate node
  elementary_type = {{"typeIdentifier", t_type}, {"typeString", type}};

  return elementary_type;
}

nlohmann::json clarity_convertert::make_array_to_pointer_type(
  const nlohmann::json &type_descrpt)
{
  // Function to replace the content of ["typeIdentifier"] with "ArrayToPtr"
  // All the information in ["typeIdentifier"] should also be available in ["typeString"]
  std::string type_identifier = "ArrayToPtr";
  std::string type_string = type_descrpt["typeString"].get<std::string>();

  std::map<std::string, std::string> m = {
    {"typeIdentifier", type_identifier}, {"typeString", type_string}};
  nlohmann::json adjusted_type = m;

  return adjusted_type;
}

nlohmann::json clarity_convertert::add_dyn_array_size_expr(
  const nlohmann::json &type_descriptor,
  const nlohmann::json &dyn_array_node)
{
  nlohmann::json adjusted_descriptor;
  adjusted_descriptor = type_descriptor;
  // get the JSON object for size expr and merge it with the original type descriptor
  adjusted_descriptor.push_back(nlohmann::json::object_t::value_type(
    "sizeExpr", dyn_array_node["initialValue"]["arguments"][0]));

  return adjusted_descriptor;
}

std::string
clarity_convertert::get_array_size(const nlohmann::json &type_descrpt)
{
  const std::string s = type_descrpt["typeString"].get<std::string>();
  std::regex rgx(".*\\[([0-9]+)\\]");
  std::string the_size;

  std::smatch match;
  if (std::regex_search(s.begin(), s.end(), match, rgx))
  {
    std::ssub_match sub_match = match[1];
    the_size = sub_match.str();
  }
  else
    assert(!"Unsupported - Missing array size in type descriptor. Detected dynamic array?");

  return the_size;
}

bool clarity_convertert::is_dyn_array(const nlohmann::json &json_in)
{
  if (json_in.contains("typeIdentifier"))
  {
    if (
      json_in["typeIdentifier"].get<std::string>().find("dyn") !=
      std::string::npos)
    {
      return true;
    }
  }
  return false;
}

// check if the child node "typeName" is a mapping
bool clarity_convertert::is_child_mapping(const nlohmann::json &ast_node)
{
  if (
    ast_node.contains("typeName") &&
    ast_node["typeName"]["nodeType"] == "Mapping")
    return true;
  return false;
}

bool clarity_convertert::get_constructor_call(
  const nlohmann::json &ast_node,
  exprt &new_expr)
{
  nlohmann::json callee_expr_json = ast_node["expression"];
  int ref_decl_id = callee_expr_json["typeName"]["referencedDeclaration"];
  exprt callee;

  const std::string contract_name = exportedSymbolsList[ref_decl_id];
  assert(linearizedBaseList.count(contract_name) && !contract_name.empty());

  const nlohmann::json constructor_ref = find_constructor_ref(ref_decl_id);

  // Special handling of implicit constructor
  // since there is no ast nodes for implicit constructor
  if (constructor_ref.empty())
    return get_implicit_ctor_ref(new_expr, contract_name);

  if (get_func_decl_ref(constructor_ref, callee))
    return true;

  // obtain the type info
  std::string id = prefix + contract_name;
  if (context.find_symbol(id) == nullptr)
    return true;

  const symbolt &s = *context.find_symbol(id);
  typet type = s.type;

  side_effect_expr_function_callt call;
  call.function() = callee;
  call.type() = type;

  auto param_nodes = constructor_ref["parameters"]["parameters"];
  unsigned num_args = 0;

  exprt new_obj("new_object");
  new_obj.type() = type;
  call.arguments().push_back(address_of_exprt(new_obj));

  for (const auto &arg : ast_node["arguments"].items())
  {
    nlohmann::json param = nullptr;
    nlohmann::json::iterator itr = param_nodes.begin();
    if (itr != param_nodes.end())
    {
      if ((*itr).contains("typeDescriptions"))
      {
        param = (*itr)["typeDescriptions"];
      }
      ++itr;
    }

    exprt single_arg;
    if (get_expr(arg.value(), param, single_arg))
      return true;

    call.arguments().push_back(single_arg);
    ++num_args;
  }

  // for adjustment
  call.set("constructor", 1);
  new_expr = call;

  return false;
}

bool clarity_convertert::get_implicit_ctor_ref(
  exprt &new_expr,
  const std::string &contract_name)
{
  // to obtain the type info
  std::string name, id;

  id = get_ctor_call_id(contract_name);
  if (context.find_symbol(id) == nullptr)
    return true;
  const symbolt &s = *context.find_symbol(id);
  typet type = s.type;

  new_expr = exprt("symbol", type);
  new_expr.identifier(id);
  new_expr.cmt_lvalue(true);
  new_expr.name(name);

  side_effect_expr_function_callt call;
  struct_typet tmp = struct_typet();
  call.function() = new_expr;
  call.type() = tmp;

  call.set("constructor", 1);
  new_expr = call;

  return false;
}

/*
  construct a void function with empty function body
  then add this function to symbol table
*/
bool clarity_convertert::get_default_function(
  const std::string name,
  const std::string id)
{
  nlohmann::json ast_node;

  auto j2 = R"(
              [
                "ParameterList",
                "ParameterList",
                0,
                {
                  "args" : []
                }
              ]
            )"_json;

  ast_node["returnParameters"] = j2;
  //std::cout << std::setw(4) << ast_node["returnParameters"] << "\n";

  code_typet type;
  if (get_type_description(ast_node["returnParameters"], type.return_type()))
    return true;

  locationt location_begin;
  current_fileName = "basic_dummy";
  if (current_fileName == "")
    return true;
  std::string debug_modulename = current_fileName;

  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, type, name, id, location_begin);

  symbol.lvalue = true;
  symbol.is_extern = false;
  symbol.file_local = false;

  symbolt &added_symbol = *move_symbol_to_context(symbol);

  code_blockt body_exprt = code_blockt();
  added_symbol.value = body_exprt;

  type.make_ellipsis();
  added_symbol.type = type;

  return false;
}

bool clarity_convertert::is_bytes_type(const typet &t)
{
  if (t.get("#clar_type").as_string().find("BYTES") != std::string::npos)
    return true;
  return false;
}

void clarity_convertert::convert_type_expr(
  const namespacet &ns,
  exprt &src_expr,
  const typet &dest_type)
{
  if (src_expr.type() != dest_type)
  {
    // only do conversion when the src.type != dest.type
    if (is_bytes_type(src_expr.type()) && is_bytes_type(dest_type))
    {
      // 1. Fixed-size Bytes Converted to Smaller Types
      //    bytes2 a = 0x4326;
      //    bytes1 b = bytes1(a); // b will be 0x43
      // 2. Fixed-size Bytes Converted to Larger Types
      //    bytes2 a = 0x4235;
      //    bytes4 b = bytes4(a); // b will be 0x42350000
      // which equals to:
      //    new_type b = bswap(new_type)(bswap(x)))

      exprt bswap_expr, sub_bswap_expr;

      // 1. bswap
      sub_bswap_expr = exprt("bswap", src_expr.type());
      sub_bswap_expr.operands().push_back(src_expr);

      // 2. typecast
      clarity_gen_typecast(ns, sub_bswap_expr, dest_type);

      // 3. bswap back
      bswap_expr = exprt("bswap", sub_bswap_expr.type());
      bswap_expr.operands().push_back(sub_bswap_expr);

      src_expr = bswap_expr;
    }
    else
      clarity_gen_typecast(ns, src_expr, dest_type);
  }
}

static inline void static_lifetime_init(const contextt &context, codet &dest)
{
  dest = code_blockt();

  // call designated "initialization" functions
  context.foreach_operand_in_order([&dest](const symbolt &s) {
    if (s.type.initialization() && s.type.is_code())
    {
      code_function_callt function_call;
      function_call.function() = symbol_expr(s);
      dest.move_to_operands(function_call);
    }
  });
}

// declare an empty array symbol and move it to the context
bool clarity_convertert::get_empty_array_ref(
  const nlohmann::json &expr,
  exprt &new_expr)
{
  // Get Name
  nlohmann::json callee_expr_json = expr["expression"];
  nlohmann::json callee_arg_json = expr["arguments"][0];

  // get unique label
  // e.g. "clar:@C@BASE@array#14"
  //TODO: FIX ME. This will probably not work in multi-contract verification.
  std::string label = std::to_string(callee_expr_json["id"].get<int>());
  std::string name, id, contract_name;
  if (get_current_contract_name(callee_expr_json, contract_name))
  {
    log_error("Internal error when obtaining the contract name. Aborting...");
    abort();
  }

  name = "array#" + label;
  if (!contract_name.empty())
    id = "clar:@C@" + contract_name + "@" + name;
  else
    id = "clar:@" + name;

  // Get Location
  locationt location_begin;
  get_location_from_decl(callee_expr_json[0], location_begin);

  // Get Debug Module Name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());

  // Get Type
  // 1. get elem type
  typet elem_type;
  const nlohmann::json elem_node =
    callee_expr_json["typeName"]["baseType"]["typeDescriptions"];
  if (get_type_description(elem_node, elem_type))
    return true;

  // 2. get array size
  exprt size;
  const nlohmann::json literal_type = callee_arg_json["typeDescriptions"];
  if (get_expr(callee_arg_json, literal_type, size))
    return true;

  // 3. declare array
  typet arr_type = array_typet(elem_type, size);

  // Get Symbol
  symbolt symbol;
  get_default_symbol(
    symbol, debug_modulename, arr_type, name, id, location_begin);

  symbol.lvalue = true;
  symbol.static_lifetime = true;
  symbol.file_local = false;
  symbol.is_extern = true;

  symbolt &added_symbol = *move_symbol_to_context(symbol);

  // Poplulate default value
  if (size.value().as_string() != "" && size.value().as_string() != "0")
  {
    added_symbol.value = gen_zero(arr_type);
    added_symbol.value.zero_initializer(true);
  }

  new_expr = symbol_expr(added_symbol);
  return false;
}

/*
  perform multi-transaction verification
  the idea is to verify the assertions that must be held
  in any function calling order.
*/
bool clarity_convertert::multi_transaction_verification(
  const std::string &contractName)
{
  /*
  convert the verifying contract to a "clar_main" function, e.g.

  Contract Base
  {
      constrcutor(){}
      function A(){}
      function B(){}
  }

  will be converted to

  void clar_main()
  {
    Base()  // constructor_call
    while(nondet_bool)
    {
      if(nondet_bool) A();
      if(nondet_bool) B();
    }
  }

  Additionally, we need to handle the inheritance. Theoretically, we need to merge (i.e. create a copy) the public and internal state variables and functions inside Base contracts into the Derive contract. However, in practice we do not need to do so. Instead, we
    - call the constructors based on the linearizedBaseList
    - add the inherited public function call to the if-body
  */

  // 0. initialize "clar_main" body and while-loop body
  codet func_body, while_body;
  static_lifetime_init(context, while_body);
  static_lifetime_init(context, func_body);

  while_body.make_block();
  func_body.make_block();

  // 1. get constructor call

  //for (auto it = id_list.rbegin(); it != id_list.rend(); ++it)
  {
    // 1.1 get contract symbol ("tag-contractName")
    std::string c_name;
    get_current_contract_name(nullptr, c_name); //exportedSymbolsList[*it];
    const std::string id = prefix + c_name;
    if (context.find_symbol(id) == nullptr)
      return true;
    const symbolt &contract = *context.find_symbol(id);
    assert(contract.type.is_struct() && "A contract should be a struct");

    // 1.2 construct a constructor call and move to func_body
    const std::string ctor_id = get_ctor_call_id(c_name);

    if (context.find_symbol(ctor_id) == nullptr)
    {
      // if the input contract name is not found in the src file, return true
      log_error("Input contract is not found in the source file.");
      return true;
    }
    const symbolt &constructor = *context.find_symbol(ctor_id);
    code_function_callt call;
    call.location() = constructor.location;
    call.function() = symbol_expr(constructor);
    const code_typet::argumentst &arguments =
      to_code_type(constructor.type).arguments();
    call.arguments().resize(
      arguments.size(), static_cast<const exprt &>(get_nil_irep()));

    // move to "clar_main" body
    func_body.move_to_operands(call);

    // 2. construct a while-loop and move to func_body

    // 2.0 check visibility setting
    bool skip_vis =
      config.options.get_option("no-visibility").empty() ? false : true;
    if (skip_vis)
    {
      log_warning(
        "force to verify every function, even it's an unreachable "
        "internal/private function. This might lead to false positives.");
    }

    // 2.1 construct ifthenelse statement
    const struct_typet::componentst &methods =
      to_struct_type(contract.type).methods();
    bool is_tgt_cnt = c_name == contractName ? true : false;

    for (const auto &method : methods)
    {
      // we only handle public (and external) function
      // as the private and internal function cannot be directly called
      if (is_tgt_cnt)
      {
        if (
          !skip_vis && method.get_access().as_string() != "public" &&
          method.get_access().as_string() != "external")
          continue;
      }
      else
      {
        // this means functions inherited from base contracts
        if (!skip_vis && method.get_access().as_string() != "public")
          continue;
      }

      // skip constructor
      const std::string func_id = method.identifier().as_string();
      if (func_id == ctor_id)
        continue;

      // guard: nondet_bool()
      if (context.find_symbol("c:@F@nondet_bool") == nullptr)
        return true;
      const symbolt &guard = *context.find_symbol("c:@F@nondet_bool");

      side_effect_expr_function_callt guard_expr;
      guard_expr.name("nondet_bool");
      guard_expr.identifier("c:@F@nondet_bool");
      guard_expr.location() = guard.location;
      guard_expr.cmt_lvalue(true);
      guard_expr.function() = symbol_expr(guard);

      // then: function_call
      if (context.find_symbol(func_id) == nullptr)
        return true;
      const symbolt &func = *context.find_symbol(func_id);
      code_function_callt then_expr;
      then_expr.location() = func.location;
      then_expr.function() = symbol_expr(func);
      const code_typet::argumentst &arguments =
        to_code_type(func.type).arguments();
      then_expr.arguments().resize(
        arguments.size(), static_cast<const exprt &>(get_nil_irep()));

      // ifthenelse-statement:
      codet if_expr("ifthenelse");
      if_expr.copy_to_operands(guard_expr, then_expr);

      // move to while-loop body
      while_body.move_to_operands(if_expr);
    }
  }

  // while-cond:
  // const symbolt &guard = *context.find_symbol("c:@F@nondet_bool");
  // side_effect_expr_function_callt cond_expr;
  // cond_expr.name("nondet_bool");
  // cond_expr.identifier("c:@F@nondet_bool");
  // cond_expr.cmt_lvalue(true);
  // cond_expr.location() = func_body.location();
  // cond_expr.function() = symbol_expr(guard);

  // // while-loop statement:
  // code_whilet code_while;
  // code_while.cond() = cond_expr;
  // code_while.body() = while_body;

  // // move to "clar_main"
  // func_body.move_to_operands(code_while);

  // 3. add "clar_main" to symbol table
  symbolt new_symbol;
  code_typet main_type;
  main_type.return_type() = empty_typet();
  const std::string clar_name = "clar_main_" + contractName;
  const std::string clar_id = "clar:@C@" + contractName + "@F@" + clar_name;
  log_status(
    "multi_transaction_verification\t: {} \nIssuer\t: {}",
    contractName,
    prefix);
  const symbolt &contract = *context.find_symbol(prefix + contractName);
  new_symbol.location = contract.location;
  std::string debug_modulename =
    get_modulename_from_path(contract.location.file().as_string());
  get_default_symbol(
    new_symbol,
    debug_modulename,
    main_type,
    clar_name,
    clar_id,
    contract.location);

  new_symbol.lvalue = true;
  new_symbol.is_extern = false;
  new_symbol.file_local = false;

  symbolt &added_symbol = *context.move_symbol_to_context(new_symbol);

  // no params
  main_type.make_ellipsis();

  added_symbol.type = main_type;
  added_symbol.value = func_body;

  // 4. set "clar_main" as main function
  // this will be overwrite in multi-contract mode.
  config.main = clar_name;

  return false;
}

/*
  This function perform multi-transaction verification on each contract in isolation.
  To do so, we construct nondetered switch_case;
*/
bool clarity_convertert::multi_contract_verification()
{
  // 0. initialize "clar_main" body and switch body
  codet func_body, switch_body;
  static_lifetime_init(context, switch_body);
  static_lifetime_init(context, func_body);

  switch_body.make_block();
  func_body.make_block();
  // 1. construct switch-case
  int cnt = 0;
  for (const auto &sym : exportedSymbolsList)
  {
    // 1.1 construct multi-transaction verification entry function
    // function "clar_main_contractname" will be created and inserted to the symbol table.
    const std::string &c_name = sym.second;
    if (linearizedBaseList.count(c_name))
    {
      if (multi_transaction_verification(c_name))
        return true;
    }
    else
      //! Assume is not a contract (e.g. error type)
      continue;

    // 1.2 construct a "case n"
    exprt case_cond = constant_exprt(
      integer2binary(cnt, bv_width(int_type())),
      integer2string(cnt),
      int_type());

    // 1.3 construct case body: entry function + break
    codet case_body;
    static_lifetime_init(context, case_body);
    case_body.make_block();

    // func_call: clar_main_contractname
    const std::string sub_clar_id =
      "clar:@C@" + c_name + "@F@clar_main_" + c_name;
    if (context.find_symbol(sub_clar_id) == nullptr)
      return true;

    const symbolt &func = *context.find_symbol(sub_clar_id);
    code_function_callt func_expr;
    func_expr.location() = func.location;
    func_expr.function() = symbol_expr(func);
    const code_typet::argumentst &arguments =
      to_code_type(func.type).arguments();
    func_expr.arguments().resize(
      arguments.size(), static_cast<const exprt &>(get_nil_irep()));
    case_body.move_to_operands(func_expr);

    // break statement
    exprt break_expr = code_breakt();
    case_body.move_to_operands(break_expr);

    // 1.4 construct case statement
    code_switch_caset switch_case;
    switch_case.case_op() = case_cond;
    convert_expression_to_code(case_body);
    switch_case.code() = to_code(case_body);

    // 1.5 move to switch body
    switch_body.move_to_operands(switch_case);

    // update case number counter
    ++cnt;
  }

  // 2. move switch to func_body
  // 2.1 construct nondet_uint jump condition
  if (context.find_symbol("c:@F@nondet_uint") == nullptr)
    return true;
  const symbolt &cond = *context.find_symbol("c:@F@nondet_uint");

  side_effect_expr_function_callt cond_expr;
  cond_expr.name("nondet_uint");
  cond_expr.identifier("c:@F@nondet_uint");
  cond_expr.location() = cond.location;
  cond_expr.cmt_lvalue(true);
  cond_expr.function() = symbol_expr(cond);

  // 2.2 construct switch statement
  code_switcht code_switch;
  code_switch.value() = cond_expr;
  code_switch.body() = switch_body;
  func_body.move_to_operands(code_switch);

  // 3. add "clar_main" to symbol table
  symbolt new_symbol;
  code_typet main_type;
  main_type.return_type() = empty_typet();
  const std::string clar_id = "clar:@F@clar_main";
  const std::string clar_name = "clar_main";

  if (
    context.find_symbol(prefix + linearizedBaseList.begin()->first) == nullptr)
    return true;
  // use first contract's location
  const symbolt &contract =
    *context.find_symbol(prefix + linearizedBaseList.begin()->first);
  new_symbol.location = contract.location;
  std::string debug_modulename =
    get_modulename_from_path(contract.location.file().as_string());
  get_default_symbol(
    new_symbol,
    debug_modulename,
    main_type,
    clar_name,
    clar_id,
    new_symbol.location);

  new_symbol.lvalue = true;
  new_symbol.is_extern = false;
  new_symbol.file_local = false;

  symbolt &added_symbol = *context.move_symbol_to_context(new_symbol);

  // no params
  main_type.make_ellipsis();

  added_symbol.type = main_type;
  added_symbol.value = func_body;
  config.main = clar_name;
  return false;
}
