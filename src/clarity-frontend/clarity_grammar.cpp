#include <fmt/core.h>
#include <clarity-frontend/clarity_grammar.h>
#include <set>
#include <util/message.h>

#define ENUM_TO_STR(s)                                                         \
  case s:                                                                      \
  {                                                                            \
    return #s;                                                                 \
  }

namespace ClarityGrammar
{
const std::unordered_map<std::string, ElementaryTypeNameT>
  uint_string_to_type_map = {
    {"uint", UINT},
};

const std::unordered_map<ElementaryTypeNameT, unsigned int> uint_size_map = {
  {UINT, 128},
  {UINT_LITERAL, 128},
};
const std::map<std::string, ElementaryTypeNameT> int_string_to_type_map = {
  {"int", INT},
};
const std::map<ElementaryTypeNameT, unsigned int> int_size_map = {
  {INT, 128},
  {INT_LITERAL, 128},
};
const std::unordered_map<std::string, ElementaryTypeNameT> bytesn_to_type_map =
  {
    {"buff", BUFF},
};
const std::map<ElementaryTypeNameT, unsigned int> bytesn_size_map = {
  {BUFF, 32},
  {UINT_LITERAL, 128},
  {INT_LITERAL, 128}};




// input    : complete ast_node
// returns  : ast_node[0] e-g "data-var" , "constant" etc
std::string get_declaration_decorator(const nlohmann::json &ast_node)
{
  return ast_node[0].get<std::string>();
}


// input    : complete ast_node
// returns   : : expression node located at ast_node[1]
nlohmann::json get_expression_node(const nlohmann::json &ast_node)
{
  return ast_node[1];
}

// input    : an expression node
// returns   : : value of ["identifier"] key as std::string
std::string get_expression_identifier(
  const nlohmann::json &ast_node)
{
  return ast_node["identifier"].get<std::string>();
  
}

// input    : an expression node
// return   :  the type of the expression node expression_node["type"]
std::string get_expression_type(
  const nlohmann::json &expression_node)
{
  return expression_node["type"].get<std::string>();
}

// input    : an expression node
// return   :  the cid of the expression node expression_node["cid"]
int get_expression_cid(const nlohmann::json &expression_node)
{
  return expression_node["cid"].get<int16_t>();
}

// input    : an expression node
// returns   :  the value (if any) of the expression node expression_node["value"] which is also an expression node
nlohmann::json get_expression_value_node(
  const nlohmann::json &expression_node)
{
  if (expression_node.contains("value"))
  {
    return expression_node["value"];
  }
  else
  {
    log_error("No value node found in the expression node");
    abort();
  }
}

// input    : an expression node
// returns   :  some/none value of the expression expression_node["identifier"] . Only applicatble to Optionals
std::string get_expression_optional_expr(const nlohmann::json &expression_node)
{
  std::string expression_type = get_expression_type(expression_node);

  if (expression_type == "optional_expression")
  {
    std::string some_none = expression_node["identifier"].get<std::string>();
    if (some_none != "some" && some_none != "none")
    {
      log_error("Invalid optional expression");
      abort();
    }
    return some_none;
  }
  else
  {
    log_error("Expression is not a literal");
    abort();
  }
}



// input    : an expression node
// returns   :  literal value of the expression expression_node["identifier"] . Only applicatble to literals.
std::string get_expression_lit_value(const nlohmann::json &expression_node)
{
  std::string expression_type = get_expression_type(expression_node);

  if (ClarityGrammar::is_literal_type(expression_type) || is_principal_declaration(expression_node))
  {
    return expression_node["identifier"].get<std::string>();
  }
  else
  {
    log_error("Expression is not a literal");
    abort();
  }
}

// input    : an expression node
// returns   :  arguments of the expression node expression_node["args"]
nlohmann::json get_expression_args(const nlohmann::json &expression_node)
{
  if (expression_node.contains("args"))
  {
    return expression_node["args"];
  }
  else
  {
    log_warning("No args node found in the expression node");
    //abort();
    return nlohmann::json::array();
  }
}

// input    : an expression node
// returns   :  objtype of the expression node expression_node["objtype"]
nlohmann::json get_expression_objtype(const nlohmann::json &expression_node)
{
  if (expression_node.contains("objtype"))
  {
    return expression_node["objtype"];
    
  }
  else
  {
   nlohmann::json expression_args = get_expression_args(expression_node);
   if (expression_args.size() > 0)
   {
      if(expression_args[0].contains("objtype"))
      {
        return expression_args[0]["objtype"];
      }
      else
      {
        nlohmann::json literal_type_expr;
        if (ClarityGrammar::get_literal_type_from_expr(expression_args[0], literal_type_expr))
        {
          log_error("Failed to get literal type from expression");
          abort();
        }
        return literal_type_expr;
      }
   }
    
    log_error("No objtype node found in the expression node");
    abort();
  }
}

// input    : objtype of the expression node
// returns   :  nested objtype if any.
nlohmann::json get_nested_objtype(const nlohmann::json &objtype)
{
  try
  {
    return objtype[3];
  }
  catch (const std::exception &e)
  {
    log_error("No nested objtype node found in the objtype node");
    abort();
  }
}

// input    : an expression node
// returns   :  body of the expression node expression_node["body"]
nlohmann::json get_expression_body(const nlohmann::json &expression_node)
{
  if (expression_node.contains("body"))
  {
    return expression_node["body"];
  }
  else
  {
    log_error("No body node found in the expression node");
    abort();
  }
  
}

// input    : an expression node
// returns   :  return type of the expression node as objtype expression_node["return_type"]
nlohmann::json get_expression_return_type(const nlohmann::json &expression_node)
{
  if (expression_node.contains("return_type"))
  {
    return expression_node["return_type"];
  }
  else
  {
    log_error("No return_type node found in the expression node");
    abort();
  }
  
}

// input    : an expression node
// output   :  Location info of the expression stored in "span" of a node expression_node["span"]
nlohmann::json  get_location_info(const nlohmann::json &expression_node)
{
  if (expression_node.contains("span"))
  {
    return expression_node["span"];
  }
  else
  {
    log_error("No span node found in the expression node");
    abort();
  }
}

// input    : an expression node
// returns   :  true if expression is a  standard principal declaration node, false otherwise
bool is_expression_standard_principal(const nlohmann::json &expression_node)
{
  std::string principal_type = get_expression_type(expression_node);
  if ((principal_type == "standard_principal") )
  {
    return true;
  }

  return false;
}


bool is_literal_type(std::string nodeType)
{
  if( (nodeType == "standard_principal") || (nodeType == "contract_principal") ||
    (nodeType == "lit_int") || (nodeType == "lit_uint") ||
    (nodeType == "lit_ascii") || (nodeType == "lit_bool") ||
    (nodeType == "lit_buffer") || (nodeType == "lit_utf8"))
  {
    return true;
  }
  return false;
}

bool is_state_variable(const std::string &ast_node_decorator)
{
  nlohmann::json ast_node_from_string = ast_node_decorator;
  return is_state_variable(ast_node_from_string);
}


bool is_state_variable(const nlohmann::json &ast_node)
{
  const std::vector<std::string> state_node_types{
    "data-var", "map", "trait", "constant", "def-ft", "def-nft"};

  if (
    std::find(state_node_types.begin(), state_node_types.end(), ast_node) !=
    state_node_types.end())
    return true;
  else
    return false;
}

bool is_tuple_declaration(const nlohmann::json &ast_node)
{
  if (ClarityGrammar::get_expression_objtype(ast_node)[0] == "tuple")
    return true;
  else
    return false;
}

bool is_response_declaration(const nlohmann::json &ast_node)
{
  if (ast_node["objtype"][0] == "response")
    return true;
  else
    return false;
}

bool is_principal_declaration(const nlohmann::json &ast_node)
{
  if (ast_node[1]["objtype"][0] == "principal")
    return true;
  else
    return false;
}

bool is_variable_declaration(const nlohmann::json &ast_node)
{
  return is_state_variable(ast_node);
}

// ml -overloaded the function to use string
bool is_variable_declaration(const std::string &ast_node_decorator)
{
  return is_state_variable(ast_node_decorator);
}

bool is_function_definition(const nlohmann::json &ast_node)
{
  const std::vector<std::string> state_node_types{
    "var-get", "read-only", "private", "public"};

  if (
    std::find(state_node_types.begin(), state_node_types.end(), ast_node) !=
    state_node_types.end())
    return true;
  else
    return false;
}

bool is_function_definition(const std::string &ast_node_decorator)
{
  nlohmann::json ast_node_from_string = ast_node_decorator;
  return is_function_definition(ast_node_from_string);
}

bool operation_is_optional_decl(const nlohmann::json &ast_node)
{
  const std::vector<std::string> optional_operators{"some", "none"};

  if (
    std::find(
      optional_operators.begin(), optional_operators.end(), ast_node[0]) !=
    optional_operators.end())
    return true;
  else
    return false;
}

bool operation_is_multiop(const nlohmann::json &ast_node)
{
  const std::vector<std::string> multiop_operators{
    "+",  "-",  "*",   "/",   "and",  "or", "bit-and",  "bit-or",
    "bit-xor", "is-eq"};

  if (
    std::find(
      multiop_operators.begin(),
      multiop_operators.end(),
      ast_node["identifier"]) != multiop_operators.end())
    return true;
  else
    return false;
}


bool operation_is_binary(const nlohmann::json &ast_node)
{
  const std::vector<std::string> binary_operators{
    "+",  "-",  "*",   "/",   "%",  "<<", ">>", "&",  "|",  ">",
    "<",  ">=", "<=",  "!=",  "==", "&&", "||", "+=", "-=", "*=",
    "/=", "%=", "<<=", ">>=", "&=", "|=", "^=", "**", "map-insert"};

  if (
    std::find(
      binary_operators.begin(),
      binary_operators.end(),
      ast_node["identifier"]) != binary_operators.end())
    return true;
  else
    return false;
}

bool operation_is_let_begin(const nlohmann::json &ast_node)
{
  const std::vector<std::string> let_begin_operators{
    "let",  "begin"};

  if (
    std::find(
      let_begin_operators.begin(),
      let_begin_operators.end(),
      ast_node["identifier"]) != let_begin_operators.end())
    return true;
  else
    return false;
}

bool operation_is_unary(const nlohmann::json &ast_node)
{
  const std::vector<std::string> unary_operators{"--", "++", "-", "~", "!"};

  if (
    std::find(unary_operators.begin(), unary_operators.end(), ast_node) !=
    unary_operators.end())
    return true;
  else
    return false;
}

bool operation_is_optional(const nlohmann::json &ast_node)
{
  const std::vector<std::string> conditional_operators{"some"};

  if (
    std::find(
      conditional_operators.begin(), conditional_operators.end(), ast_node) !=
    conditional_operators.end())
    return true;
  else
    return false;
}

bool operation_is_conditional(const nlohmann::json &ast_node)
{
  const std::vector<std::string> conditional_operators{"if"};

  if (
    std::find(
      conditional_operators.begin(),
      conditional_operators.end(),
      ast_node["identifier"]) != conditional_operators.end())
    return true;
  else
    return false;
}

// takes objtype node as input
// returns symbolid of the optional struct w.r.t to the objtype passed
std::string get_optional_symbolId(const nlohmann::json &optional_type)
{
  ElementaryTypeNameT optional_typet =
    get_elementary_type_name_t(optional_type);
  std::string symbol_id;

  switch (optional_typet)
  {
  case ElementaryTypeNameT::INT:
    symbol_id = "tag-struct optional_int128_t";
    break;
  case ElementaryTypeNameT::UINT:
    symbol_id = "tag-struct optional_uint128_t";
    break;
  case ElementaryTypeNameT::BOOL:
    symbol_id = "tag-struct optional_bool";
    break;
  case ElementaryTypeNameT::STRING_ASCII:
    symbol_id = "tag-struct optional_string";
    break;
  case ElementaryTypeNameT::STRING_UTF8:
    symbol_id = "tag-struct optional_string";
    break;
  case ElementaryTypeNameT::BUFF:
    symbol_id = "tag-struct optional_buff";
    break;
  default:
    log_error("Unimplemented optional type");
    abort();
  }
  return symbol_id;
}


// takes objtype as input
// returns objtype for optional inside an objtype
// Description : if objtype passed is of the parent expression node of an optional,
// this this function will assume the lenght of objtype is greater than 3, 
// because objtype will have a nested objtype inside it.
// However, if the objtype length is less than 3, then it's a direct objtype
// and should be returned as is.
nlohmann::json get_optional_type(const nlohmann::json &objtype)
{
  if (objtype.size() > 3)
    return get_nested_objtype(objtype);
  else
    return objtype;
  
}

bool get_operation_type(nlohmann::json &expression_node)
{
  nlohmann::json value_node = expression_node[1]["value"];

  if (operation_is_binary(value_node))
  {
    expression_node[1]["expressionType"] = "BinaryOperation";
  }
  else if (operation_is_unary(value_node))
  {
    expression_node[1]["expressionType"] = "UnaryOperation";
  }
  else if (operation_is_conditional(value_node))
  {
    expression_node[1]["expressionType"] = "Conditional";
  }
  else if (value_node[0] == "tuple")
  {
    expression_node[1]["expressionType"] = "TupleExpression";
  }
  else if (operation_is_optional_decl(value_node))
  {
    expression_node[1]["expressionType"] = "Optional";

    //expression_node[1]
  }
  else if (value_node[0] == "principal")
  {
    expression_node[1]["expressionType"] = "Literal";

    auto principal_value = value_node[3]["value"];
    std::string principal_value_str;
    if (principal_value.size() > 22) //indicates it's a contract principal
    {
      // confirming contract principal by looking for "." in the principal string name
      principal_value_str = value_node[3]["value"][22];
      if (principal_value_str.find(".") != std::string::npos)
      {
        size_t period_pos = principal_value_str.find(".");
        expression_node[1]["principalType"] = "contract";
        expression_node[1]["contractName"] = value_node[3]["value"][21];
        expression_node[1]["issuerPrincipal"] =
          principal_value_str.substr(0, period_pos);
      }
      else
      {
        return true;
      }
    }
    else
    {
      expression_node[1]["principalType"] = "standard";
      expression_node[1]["issuerPrincipal"] = value_node[3]["value"][21];
      expression_node[1]["contractName"] = "Not a contract principal";
    }
    return false;
  }
  else if (value_node[0] == "list")
  {
    expression_node[1]["expressionType"] = "List";
  }
  else
  {
    ///log_error("Unsupported operation type: {}", value_node[0]);
    return true; // unexpected
  }

  return false;

  /*
  else if (nodeType == "TupleExpression")
  {
    return Tuple;
  }
  else if (nodeType == "Mapping")
  {
    return Mapping;
  }
  else if (nodeType == "FunctionCall")
    
    */
}

// input    : The json expr type which contain the identifier
// output   : The objtype json from the expr type
// returns  : false if succesful, or true if failed.
bool get_literal_type_from_expr(
  const nlohmann::json &expr,
  nlohmann::json &expression_node)
{
  std::string expr_type = get_expression_type(expr);

  if (expr_type == "lit_uint")
  {
    auto j2 = R"(
            ["uint", "uint_128", "128"]              
          )"_json;
    expression_node = j2;
  }
  else if (expr_type == "lit_int")
  {
    auto j2 = R"(
            ["int", "int_128", "128"]              
          )"_json;
    expression_node = j2;
  }
  else if (expr_type == "lit_bool")
  {
    auto j2 = R"(
            ["bool", "bool", "1"]              
          )"_json;
    expression_node = j2;
  }
  else if (expr_type == "lit_buffer")
  {
    auto j2 = R"(
            ["buffer", "buffer", "4"]              
          )"_json;
    expression_node = j2;
  }
  else if (expr_type == "lit_utf8")
  {
    auto j2 = R"(
            ["string-utf8", "string-utf8", "16"]              
          )"_json;
    expression_node = j2;
  }
  else if (expr_type == "lit_ascii")
  {
    std::string literal_string = get_expression_identifier(expr);
    std::string literal_string_length = std::to_string(literal_string.length());

    expression_node = nlohmann::json::array(
      {"string-ascii", "string-ascii", literal_string_length});
  }
  else
  {
    log_error("Unsupported operation type: {}", expr_type);
    return true; // unexpected
  }

  return false;

  /*
  else if (nodeType == "TupleExpression")
  {
    return Tuple;
  }
  else if (nodeType == "Mapping")
  {
    return Mapping;
  }
  else if (nodeType == "FunctionCall")
    
    */
}

bool parse_value_node(nlohmann::json &expression_node)
{
  // parse value node
  nlohmann::json value_node = expression_node[1]["value"];
  std::string value_type = value_node.type_name();

  if (
    value_type == "string" || value_type == "number" || value_type == "object")
  {
    // it's a literal value
    //
    expression_node[1]["expressionType"] = "Literal";
    if (value_type == "object")
    {
      // look for if it has "lit_ascii" or "lit_utf8" as keys inside it. and take those values.
    }
    else
    {
    }
  }
  else if (value_type == "array")
  {
    // it's a function call with arguments

    if (get_operation_type(expression_node))
    {
      return true;
    }
  }
  else
  {
    log_error("Unsupported value type: {}", value_type);
    return false;
  }

  return false;
}

bool parse_expression_element(nlohmann::json &expr_element_json)
{
  std::string expression_class_decorator = ClarityGrammar::get_declaration_decorator(expr_element_json);

  // determine if it's a variable / constant declaration or a function definition
  bool var_decl = is_variable_declaration(expression_class_decorator);
  bool func_def = is_function_definition(expression_class_decorator);

  // add a nodeType for easier differenciation down the line.
  if (var_decl)
  {
    //return process_variable_declaration(expr_element_json);
    expr_element_json[1]["nodeType"] = "VariableDeclaration";
  }
  else if (func_def)
  {
    //return process_function_definition(expr_element_json);
    expr_element_json[1]["nodeType"] = "FunctionDefinition";
    // do not process yet
    return false;
  }
  else
  {
    log_error("Unsupported expression class: {}", expression_class_decorator);
    return true;
  }

  // parse value node
  parse_value_node(expr_element_json);

  return false;
}
// rule contract-body-element
ContractBodyElementT get_contract_body_element_t(const nlohmann::json &element)
{
  std::string element_type = get_expression_type(element);
  if (
    (element_type == "variable_declaration") ||
    (element_type == "constant_declaration") ||
    (element_type == "map_declaration")
    )

  {
    return VarDecl;
  }
  else if (element_type == "function_declaration")
  {
    return FunctionDef;
  }
  else if (element_type == "native_function")
  {
    return TopLevelNativeFunction;
  }
  else
  {
    log_error(
      "Got contract-body-element nodeType={}. Unsupported "
      "contract-body-element type",
      element["type"].get<std::string>());
    abort();
  }
  return ContractBodyElementTError;
}

const char *contract_body_element_to_str(ContractBodyElementT type)
{
  switch (type)
  {
    ENUM_TO_STR(VarDecl)
    ENUM_TO_STR(FunctionDef)
    ENUM_TO_STR(ContractBodyElementTError)
  default:
  {
    assert(!"Unknown contract-body-element type");
    return "UNKNOWN";
  }
  }
}

// rule type-name
// process objType
TypeNameT get_type_name_t(const nlohmann::json &type_name)
{
  // Clarity AST node has type stored in ast_node[1]["objtype"] as [ "typeName","typeIdentifier" , "size"]
  //! Order matters

  const std::string typeString = type_name[0]; //type name
  const std::string typeIdentifier =
    ""; //FIXME: we can't have type_name[1];  as it's not valid for tuples

  if (typeString != "ParameterList") // if (type_name.contains("typeString"))
  {
    //type_name["typeIdentifier"].get<std::string>();

    // we must first handle tuple
    // otherwise we might parse tuple(literal_string, literal_string)
    // as ElementaryTypeName
    if (typeString == "tuple")
    {
      return TupleTypeName;
    }
  
    else if (typeString == "buffer")
    {
      //buff in clarity can be considered as array of bytes

      return BuffTypeName;
    }
    else if (typeString == "response")
    {

      return ResponseTypeName;
    }
    else if (typeString == "map")
    {
      return MapTypeName;
    }
    else if (typeString == "list")
    {
      //list in clarity can be considered as array of items

      return ListTypeName;
    }
    else if (
      uint_string_to_type_map.count(typeString) ||
      int_string_to_type_map.count(typeString) || typeString == "bool" ||
      typeString == "string-ascii" || typeString == "string-utf8" ||
      typeString == "principal")
    {
      // For state var declaration,
      return ElementaryTypeName;
    }
    else if (typeIdentifier.find("t_contract$") != std::string::npos)
    {
      return ContractTypeName;
    }
    else if (typeString == "optional")
    {
      // For type conversion
      return OptionalTypeName;
    }
    else if (typeString.find("int_const") != std::string::npos)
    {
      // For Literal, their typeString is like "int_const 100".
      return ElementaryTypeName;
    }
    // for Special Variables and Functions
    else if (typeIdentifier.compare(0, 7, "t_magic") == 0)
    {
      return BuiltinTypeName;
    }
    else if (typeString.compare(0, 11, "return_type") == 0)
    {
      return ReturnTypeName;
    }
    else
    {
      log_error(
        "Got type-name typeString={}. Unsupported type-name type", typeString);
      abort();
    }
  }
  else
  {
    // for AST node that does not contain ["typeDescriptions"] only
    // function returnParameters

    return ParameterList;

    // else
    // {
    //   log_error(
    //     "Got type-name nodeType={}. Unsupported type-name type",
    //     type_name["nodeType"].get<std::string>());
    //   abort();
    // }
  }

  return TypeNameTError; // to make some old compiler happy
}

const char *type_name_to_str(TypeNameT type)
{
  switch (type)
  {
    ENUM_TO_STR(ElementaryTypeName)
    ENUM_TO_STR(ParameterList)
    ENUM_TO_STR(ListTypeName)
    ENUM_TO_STR(ContractTypeName)
    ENUM_TO_STR(OptionalTypeName)
    ENUM_TO_STR(TupleTypeName)
    ENUM_TO_STR(MapTypeName)
    ENUM_TO_STR(BuiltinTypeName)
    ENUM_TO_STR(ReturnTypeName)
    ENUM_TO_STR(TypeNameTError)
  default:
  {
    assert(!"Unknown type-name type");
    return "UNKNOWN";
  }
  }
}

// rule elementary-type-name
// return the type of expression
// takes objtype node as input
ElementaryTypeNameT get_elementary_type_name_t(const nlohmann::json &type_name)
{
  std::string typeString =
    type_name[0]; //type_name["typeString"].get<std::string>();
  // rule unsigned-integer-type

  if (uint_string_to_type_map.count(typeString))
  {
    return uint_string_to_type_map.at(typeString);
  }
  if (int_string_to_type_map.count(typeString))
  {
    return int_string_to_type_map.at(typeString);
  }
  if (typeString == "bool")
  {
    return BOOL;
  }
  if (typeString.find("uint_const") != std::string::npos)
  {
    /**
     * For Literal, their typeString is like "int_const 100".
     * There is no additional type info (bitsize, signed/unsigned),
     * This means it will require additional type info from the parent
     * expr to create an internal type.
     */
    return UINT_LITERAL;
  }
  if (typeString.find("int_const") != std::string::npos)
  {
    /**
     * For Literal, their typeString is like "int_const 100".
     * There is no additional type info (bitsize, signed/unsigned),
     * This means it will require additional type info from the parent
     * expr to create an internal type.
     */
    return INT_LITERAL;
  }
  //if (typeString.find("literal_ascii_string") == 0)
  if (typeString.find("literal_ascii_string") == 0)
  {
    return STRING_ASCII_LITERAL;
  }
  if (typeString.find("literal_utf8_string") == 0)
  {
    return STRING_UTF8_LITERAL;
  }
  if (typeString == "string-ascii")
  {
    return STRING_ASCII;
  }
  if (typeString == "string-utf8")
  {
    return STRING_UTF8;
  }
  if (typeString == "principal")
  {
    return PRINCIPAL;
  }
  if (bytesn_to_type_map.count(typeString))
  {
    // fixed-size arrays bytesN, where N is a number between 1 and 32
    return bytesn_to_type_map.at(typeString);
  }
  if (typeString.find("buff") != std::string::npos)
  {
    return BUFF;
  }
  log_error(
    "Got elementary-type-name typeString={}. Unsupported "
    "elementary-type-name type",
    type_name["typeString"].get<std::string>());
  abort();
}

const char *elementary_type_name_to_str(ElementaryTypeNameT type)
{
  switch (type)
  {
    ENUM_TO_STR(UINT)
    ENUM_TO_STR(UINT_LITERAL)
    ENUM_TO_STR(INT)
    ENUM_TO_STR(INT_LITERAL)
    ENUM_TO_STR(BOOL)
    ENUM_TO_STR(PRINCIPAL)
    ENUM_TO_STR(STRING_ASCII)
    ENUM_TO_STR(STRING_ASCII_LITERAL)
    ENUM_TO_STR(STRING_UTF8)
    ENUM_TO_STR(STRING_UTF8_LITERAL)
    ENUM_TO_STR(BUFF)
    ENUM_TO_STR(BUFF_LITERAL)
    ENUM_TO_STR(ERROR)

    ENUM_TO_STR(ElementaryTypeNameTError)
  default:
  {
    assert(!"Unknown elementary-type-name type");
    return "UNKNOWN";
  }
  }
}

unsigned int uint_type_name_to_size(ElementaryTypeNameT type)
{
  return uint_size_map.at(type);
}

unsigned int int_type_name_to_size(ElementaryTypeNameT type)
{
  return int_size_map.at(type);
}

unsigned int bytesn_type_name_to_size(ElementaryTypeNameT type)
{
  return bytesn_size_map.at(type);
}

// rule parameter-list
ParameterListT get_parameter_list_t(const nlohmann::json &type_name)
{
  if (type_name["args"].size() == 0)
  {
    return EMPTY;
  }
  else if (type_name["args"].size() == 1)
  {
    return ONE_PARAM;
  }
  else if (type_name["args"].size() > 1)
  {
    return MORE_THAN_ONE_PARAM;
  }

  return ParameterListTError; // to make some old gcc compilers happy
}

const char *parameter_list_to_str(ParameterListT type)
{
  switch (type)
  {
    ENUM_TO_STR(EMPTY)
    ENUM_TO_STR(ONE_PARAM)
    ENUM_TO_STR(MORE_THAN_ONE_PARAM)
    ENUM_TO_STR(ParameterListTError)
  default:
  {
    assert(!"Unknown parameter-list type");
    return "UNKNOWN";
  }
  }
}

// function body
FuncBodyT get_function_body_t(const nlohmann::json &body)
{
  
  if ((body.is_array()) && (body.size() == 1))
  {
    return SingleStatement;
  }
  else if ((body.is_array()) && (body.size() > 1))
  {
    return MultipleStatement;
  }
  else
  {
    log_error(
      "Got function block nodeType={}. Unsupported block type", body.dump());
    abort();
  }
  return FuncBodyTError;
}

const char *function_body_to_str(FuncBodyT type)
{
  switch (type)
  {
    ENUM_TO_STR(SingleStatement)
    ENUM_TO_STR(MultipleStatement)
    ENUM_TO_STR(FuncBodyTError)
  default:
  {
    assert(!"Unknown function body type");
    return "UNKNOWN";
  }
  }
}

// rule block
BlockT get_block_t(const nlohmann::json &block)
{
  if (block["nodeType"] == "Block" && block.contains("statements"))
  {
    return Statement;
  }
  else if (block["nodeType"] == "ForStatement")
  {
    return BlockForStatement;
  }
  else if (block["nodeType"] == "IfStatement")
  {
    return BlockIfStatement;
  }
  else if (block["nodeType"] == "ExpressionStatement")
  {
    return BlockExpressionStatement;
  }
  else
  {
    log_error(
      "Got block nodeType={}. Unsupported block type",
      block["nodeType"].get<std::string>());
    abort();
  }
  return BlockTError;
}

const char *block_to_str(BlockT type)
{
  switch (type)
  {
    ENUM_TO_STR(Statement)
    ENUM_TO_STR(BlockForStatement)
    ENUM_TO_STR(BlockIfStatement)
    ENUM_TO_STR(BlockExpressionStatement)
    ENUM_TO_STR(UncheckedBlock)
    ENUM_TO_STR(BlockTError)
  default:
  {
    assert(!"Unknown block type");
    return "UNKNOWN";
  }
  }
}

// rule statement
StatementT get_statement_t(const nlohmann::json &stmt)
{
  if (stmt["nodeType"] == "ExpressionStatement")
  {
    return ExpressionStatement;
  }
  else if (stmt["nodeType"] == "VariableDeclarationStatement")
  {
    return VariableDeclStatement;
  }
  else if (stmt["nodeType"] == "Return")
  {
    return ReturnStatement;
  }
  else if (stmt["nodeType"] == "ForStatement")
  {
    return ForStatement;
  }
  else if (stmt["nodeType"] == "Block")
  {
    return Block;
  }
  else if (stmt["nodeType"] == "IfStatement")
  {
    return IfStatement;
  }
  else if (stmt["nodeType"] == "Continue")
  {
    return ContinueStatement;
  }
  else if (stmt["nodeType"] == "Break")
  {
    return BreakStatement;
  }
  else if (stmt["nodeType"] == "RevertStatement")
  {
    return RevertStatement;
  }

  else
  {
    log_error(
      "Got statement nodeType={}. Unsupported statement type",
      stmt["nodeType"].get<std::string>());
    abort();
  }
  return StatementTError;
}

const char *statement_to_str(StatementT type)
{
  switch (type)
  {
    ENUM_TO_STR(Block)
    ENUM_TO_STR(ExpressionStatement)
    ENUM_TO_STR(VariableDeclStatement)
    ENUM_TO_STR(ReturnStatement)
    ENUM_TO_STR(ForStatement)
    ENUM_TO_STR(IfStatement)
    ENUM_TO_STR(StatementTError)
    ENUM_TO_STR(ContinueStatement)
    ENUM_TO_STR(BreakStatement)
    ENUM_TO_STR(RevertStatement)
  default:
  {
    assert(!"Unknown statement type");
    return "UNKNOWN";
  }
  }
}

// rule expression
ExpressionT get_expression_t(const nlohmann::json &expr)
{
  if (expr.is_null())
  {
    return NullExpr;
  }

  std::string nodeType = get_expression_type(expr);

  // if (nodeType == "Assignment" || nodeType == "BinaryOperation")
  // {
  //   return BinaryOperatorClass;
  // }
  // else if (nodeType == "UnaryOperation")
  // {
  //   return UnaryOperatorClass;
  // }
  // else if (nodeType == "Conditional")
  // {
  //   return ConditionalOperatorClass;
  // }
  if (nodeType == "native_function")
  {
    if (operation_is_multiop(expr))
    {
      return MultiOperatorClass;
    }
    else if (operation_is_binary(expr))
    {
      return BinaryOperatorClass;
    }
    else if (operation_is_let_begin(expr))
    {
      return LetBeginDeclaration;
    }
    else 
    {
      // ml- if the operation is not binary 
      // then its a clarity built in function
      return CallExprClass;
    }
  }
  else if (nodeType == "conditional_expression")
  {
    if (operation_is_conditional(expr))
    {
      return ConditionalOperatorClass;
    }
  }
  else if (nodeType == "variable")
  {
    return DeclRefExprClass;
  }
  else if (is_literal_type(nodeType))
  {
    return Literal;
  }
  else if (nodeType == "let_variable_declaration")
  {
    return LetVariableDecl;
  }  
  else if (nodeType == "optional_expression")
  {
    return Optional;
  }
  else if (nodeType == "response_expression")
  {
    return Response;
  }
  else if (nodeType == "tuple_object")
  {
    return Tuple;
  }
  else if (nodeType == "list")
  {
    return List;
  }
  else if (nodeType == "map_declaration")
  {
    return Mapping;
  }
  

 

  // else if (nodeType == "Mapping")
  // {
  //   return Mapping;
  // }
  else if (nodeType == "user_function")
  {
    // if (expr["expression"]["nodeType"] == "NewExpression")
    //   return NewExpression;
    // if (
    //   expr["expression"]["nodeType"] == "ElementaryTypeNameExpression" &&
    //   expr["kind"] == "typeConversion")
    //   return ElementaryTypeNameExpression;
    return CallExprClass;
  }
  // else if (nodeType == "MemberAccess")
  // {
  //   assert(expr.contains("expression"));
  //   ClarityGrammar::TypeNameT type_name =
  //     get_type_name_t(expr["expression"]["typeDescriptions"]);
  //   if (type_name == ClarityGrammar::TypeNameT::ContractTypeName)
  //     return ContractMemberCall;
  //   else
  //     //TODO Assume it's a builtin member
  //     // due to that the BuiltinTypeName cannot cover all the builtin member
  //     // e.g. string.concat ==> TypeConversionName
  //     return BuiltinMemberCall;
  // }
  // else if (nodeType == "ImplicitCastExprClass")
  // {
  //   return ImplicitCastExprClass;
  // }
  // else if (nodeType == "IndexAccess")
  // {
  //   return IndexAccess;
  // }
  else
  {
    log_error(
      "Got expression nodeType={}. Unsupported expression type", nodeType);
    abort();
  }
  return ExpressionTError;
}

ExpressionT get_unary_expr_operator_t(const nlohmann::json &expr, bool uo_pre)
{
  if (expr["identifier"] == "--")
  {
    if (uo_pre)
      return UO_PreDec;
    else
      return UO_PostDec;
  }
  else if (expr["identifier"] == "++")
  {
    if (uo_pre)
      return UO_PreInc;
    else
      return UO_PostInc;
  }
  else if (expr["identifier"] == "-")
  {
    return UO_Minus;
  }
  else if (expr["identifier"] == "~")
  {
    return UO_Not;
  }
  else if (expr["identifier"] == "!")
  {
    return UO_LNot;
  }
  else
  {
    log_error(
      "Got expression operator={}. Unsupported expression operator",
      expr["identifier"].get<std::string>());

    abort();
  }
}

ExpressionT get_expr_operator_t(const nlohmann::json &expr)
{
  if ((expr["identifier"] == "=") || (expr["identifier"] == "map-insert"))
  {
    return BO_Assign;
  }
  else if (expr["identifier"] == "+")
  {
    return BO_Add;
  }
  else if (expr["identifier"] == "-")
  {
    return BO_Sub;
  }
  else if (expr["identifier"] == "*")
  {
    return BO_Mul;
  }
  else if (expr["identifier"] == "/")
  {
    return BO_Div;
  }
  else if (expr["identifier"] == "%")
  {
    return BO_Rem;
  }
  else if (expr["identifier"] == "<<")
  {
    return BO_Shl;
  }
  else if (expr["identifier"] == ">>")
  {
    return BO_Shr;
  }
  else if (expr["identifier"] == "bit-and")
  {
    return BO_And;
  }
  else if (expr["identifier"] == "bit-xor")
  {
    return BO_Xor;
  }
  else if (expr["identifier"] == "bit-or")
  {
    return BO_Or;
  }
  else if (expr["identifier"] == ">")
  {
    return BO_GT;
  }
  else if (expr["identifier"] == "<")
  {
    return BO_LT;
  }
  else if (expr["identifier"] == ">=")
  {
    return BO_GE;
  }
  else if (expr["identifier"] == "<=")
  {
    return BO_LE;
  }
  else if (expr["identifier"] == "!=")
  {
    return BO_NE;
  }
  else if (expr["identifier"] == "is-eq")
  {
    return BO_EQ;
  }
  else if (expr["identifier"] == "and")
  {
    return BO_LAnd;
  }
  else if (expr["identifier"] == "or")
  {
    return BO_LOr;
  }
  else if (expr["identifier"] == "+=")
  {
    return BO_AddAssign;
  }
  else if (expr["identifier"] == "-=")
  {
    return BO_SubAssign;
  }
  else if (expr["identifier"] == "*=")
  {
    return BO_MulAssign;
  }
  else if (expr["identifier"] == "/=")
  {
    return BO_DivAssign;
  }
  else if (expr["identifier"] == "%=")
  {
    return BO_RemAssign;
  }
  else if (expr["identifier"] == "<<=")
  {
    return BO_ShlAssign;
  }
  else if (expr["identifier"] == ">>=")
  {
    return BO_ShrAssign;
  }
  else if (expr["identifier"] == "&=")
  {
    return BO_AndAssign;
  }
  else if (expr["identifier"] == "^=")
  {
    return BO_XorAssign;
  }
  else if (expr["identifier"] == "|=")
  {
    return BO_OrAssign;
  }
  else if (expr["identifier"] == "**")
  {
    return BO_Pow;
  }
  else
  {
    log_error(
      "Got expression operator={}. Unsupported expression operator",
      expr["identifier"].get<std::string>());
    abort();
  }

  return ExpressionTError; // make some old compilers happy
}

const char *expression_to_str(ExpressionT type)
{
  switch (type)
  {
    ENUM_TO_STR(BinaryOperatorClass)
    ENUM_TO_STR(MultiOperatorClass)
    ENUM_TO_STR(BO_Assign)
    ENUM_TO_STR(BO_Add)
    ENUM_TO_STR(BO_Sub)
    ENUM_TO_STR(BO_Mul)
    ENUM_TO_STR(BO_Div)
    ENUM_TO_STR(BO_Rem)

    ENUM_TO_STR(BO_Shl)
    ENUM_TO_STR(BO_Shr)
    ENUM_TO_STR(BO_And)
    ENUM_TO_STR(BO_Xor)
    ENUM_TO_STR(BO_Or)

    ENUM_TO_STR(BO_GT)
    ENUM_TO_STR(BO_LT)
    ENUM_TO_STR(BO_GE)
    ENUM_TO_STR(BO_LE)
    ENUM_TO_STR(BO_NE)
    ENUM_TO_STR(BO_EQ)
    ENUM_TO_STR(BO_LAnd)
    ENUM_TO_STR(BO_LOr)

    ENUM_TO_STR(BO_AddAssign)
    ENUM_TO_STR(BO_SubAssign)
    ENUM_TO_STR(BO_MulAssign)
    ENUM_TO_STR(BO_DivAssign)
    ENUM_TO_STR(BO_RemAssign)
    ENUM_TO_STR(BO_ShlAssign)
    ENUM_TO_STR(BO_ShrAssign)
    ENUM_TO_STR(BO_AndAssign)
    ENUM_TO_STR(BO_XorAssign)
    ENUM_TO_STR(BO_OrAssign)
    ENUM_TO_STR(BO_Pow)

    ENUM_TO_STR(UnaryOperatorClass)
    ENUM_TO_STR(UO_PreDec)
    ENUM_TO_STR(UO_PreInc)
    ENUM_TO_STR(UO_PostDec)
    ENUM_TO_STR(UO_PostInc)
    ENUM_TO_STR(UO_Minus)
    ENUM_TO_STR(UO_Not)
    ENUM_TO_STR(UO_LNot)

    ENUM_TO_STR(ConditionalOperatorClass)

    ENUM_TO_STR(DeclRefExprClass)
    ENUM_TO_STR(Literal)
    ENUM_TO_STR(Tuple)
    ENUM_TO_STR(Mapping)
    ENUM_TO_STR(CallExprClass)
    ENUM_TO_STR(LetBeginDeclaration)
    ENUM_TO_STR(LetVariableDecl)
    ENUM_TO_STR(ImplicitCastExprClass)
    ENUM_TO_STR(IndexAccess)
    ENUM_TO_STR(NewExpression)
    ENUM_TO_STR(ContractMemberCall)
    ENUM_TO_STR(StructMemberCall)
    ENUM_TO_STR(EnumMemberCall)
    ENUM_TO_STR(BuiltinMemberCall)
    ENUM_TO_STR(ElementaryTypeNameExpression)
    ENUM_TO_STR(NullExpr)
    ENUM_TO_STR(ExpressionTError)
    ENUM_TO_STR(Optional)
    ENUM_TO_STR(List)
    ENUM_TO_STR(Response)
  default:
  {
    assert(!"Unknown expression type");
    return "UNKNOWN";
  }
  }
}

// rule variable-declaration-statement
VarDeclStmtT get_var_decl_stmt_t(const nlohmann::json &stmt)
{
  if (
    stmt["nodeType"] == "VariableDeclaration" && stmt["stateVariable"] == false)
  {
    return VariableDecl;
  }
  else
  {
    log_error(
      "Got expression nodeType={}. Unsupported "
      "variable-declaration-statement operator",
      stmt["nodeType"].get<std::string>());
    abort();
  }
  return VarDeclStmtTError; // make some old compilers happy
}

const char *var_decl_statement_to_str(VarDeclStmtT type)
{
  switch (type)
  {
    ENUM_TO_STR(VariableDecl)
    ENUM_TO_STR(VariableDeclTuple)
    ENUM_TO_STR(VarDeclStmtTError)
  default:
  {
    assert(!"Unknown variable-declaration-statement type");
    return "UNKNOWN";
  }
  }
}

// auxiliary type to convert function call
FunctionDeclRefT get_func_decl_ref_t(const nlohmann::json &decl)
{
  assert(decl["nodeType"] == "FunctionDefinition");
  if (
    decl["parameters"]["parameters"].size() == 0 ||
    decl["kind"] == "constructor")
  {
    return FunctionNoProto;
  }
  else
  {
    return FunctionProto;
  }
  return FunctionDeclRefTError; // to make some old compilers happy
}

const char *func_decl_ref_to_str(FunctionDeclRefT type)
{
  switch (type)
  {
    ENUM_TO_STR(FunctionProto)
    ENUM_TO_STR(FunctionNoProto)
    ENUM_TO_STR(FunctionDeclRefTError)
  default:
  {
    assert(!"Unknown auxiliary type to convert function call");
    return "UNKNOWN";
  }
  }
}

// auxiliary type for implicit casting
ImplicitCastTypeT get_implicit_cast_type_t(std::string cast)
{
  if (cast == "LValueToRValue")
  {
    return LValueToRValue;
  }
  else if (cast == "FunctionToPointerDecay")
  {
    return FunctionToPointerDecay;
  }
  else if (cast == "ArrayToPointerDecay")
  {
    return ArrayToPointerDecay;
  }
  else
  {
    log_error("Got implicit cast type={}. Unsupported case type", cast.c_str());
    abort();
  }

  return ImplicitCastTypeTError; // to make some old compilers happy
}

const char *implicit_cast_type_to_str(ImplicitCastTypeT type)
{
  switch (type)
  {
    ENUM_TO_STR(LValueToRValue)
    ENUM_TO_STR(FunctionToPointerDecay)
    ENUM_TO_STR(ArrayToPointerDecay)
    ENUM_TO_STR(ImplicitCastTypeTError)
  default:
  {
    assert(!"Unknown auxiliary type for implicit casting");
    return "UNKNOWN";
  }
  }
}

VisibilityT get_access_t(const nlohmann::json &ast_node)
{
  std::string access = ast_node["visibility"].get<std::string>();
  if (access == "public")
  {
    return PublicT;
  }
  else if (access == "private")
  {
    return PrivateT;
  }
  else
  {
    log_error("Unknown Visibility");
    abort();
  }
}

}; // namespace ClarityGrammar
