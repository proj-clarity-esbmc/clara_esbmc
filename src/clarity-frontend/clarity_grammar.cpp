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
  if (ast_node["objtype"][0] == "tuple")
    return true;
  else
    return false;
}

bool is_variable_declaration(const nlohmann::json &ast_node)
{
  return is_state_variable(ast_node);
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

bool operation_is_binary(const nlohmann::json &ast_node)
{
  const std::vector<std::string> binary_operators{
    "+",  "-",  "*",   "/",   "%",  "<<", ">>", "&",  "|",  ">",
    "<",  ">=", "<=",  "!=",  "==", "&&", "||", "+=", "-=", "*=",
    "/=", "%=", "<<=", ">>=", "&=", "|=", "^=", "**"};

  if (
    std::find(binary_operators.begin(), binary_operators.end(), ast_node) !=
    binary_operators.end())
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
      conditional_operators.begin(),
      conditional_operators.end(),
      ast_node) != conditional_operators.end())
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
      ast_node) != conditional_operators.end())
    return true;
  else
    return false;
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
  std::string expression_class = expr_element_json[0];

  // determine if it's a variable / constant declaration or a function definition
  bool var_decl = is_variable_declaration(expr_element_json[0]);
  bool func_def = is_function_definition(expr_element_json[0]);
  
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
    log_error("Unsupported expression class: {}", expression_class);
    return true;
  }

  // parse value node
  parse_value_node(expr_element_json);

  return false;
}
// rule contract-body-element
ContractBodyElementT get_contract_body_element_t(const nlohmann::json &element)
{
  if (element[1]["type"] == "variable") 
  {
    return VarDecl;
  }
  else if (
    element[1]["type"] == "function_name")
  {
    return FunctionDef;
  }
  else
  {
    log_error(
      "Got contract-body-element nodeType={}. Unsupported "
      "contract-body-element type",
      element["nodeType"].get<std::string>());
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
    if (typeIdentifier.compare(0, 10, "t_mapping(") == 0)
    {
      return MappingTypeName;
    }
    else if (typeString == "buffer")
    {
     //buff in clarity can be considered as array of bytes

      return BuffTypeName;
    }
    else if (typeString == "list")
    {
      //list in clarity can be considered as array of bytes

      return ArrayTypeName;
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
    else if (typeString.find("type(") != std::string::npos)
    {
      // For type conversion
      return TypeConversionName;
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
        "Got type-name typeString={}. Unsupported type-name type",
        typeString);
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
    ENUM_TO_STR(ArrayTypeName)
    ENUM_TO_STR(ContractTypeName)
    ENUM_TO_STR(TypeConversionName)
    ENUM_TO_STR(TupleTypeName)
    ENUM_TO_STR(MappingTypeName)
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
    // TODO
    return STRING_ASCII;
  }
  if (typeString == "string-utf8")
  {
    // TODO
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

  std::string nodeType = expr[1]["expressionType"];

  if (nodeType == "Assignment" || nodeType == "BinaryOperation")
  {
    return BinaryOperatorClass;
  }
  else if (nodeType == "UnaryOperation")
  {
    return UnaryOperatorClass;
  }
  else if (nodeType == "Conditional")
  {
    return ConditionalOperatorClass;
  }
  else if (nodeType == "Identifier" && expr.contains("referencedDeclaration"))
  {
    return DeclRefExprClass;
  }
  else if (nodeType == "Literal")
  {
    return Literal;
  }
  else if (nodeType == "TupleExpression")
  {
    return Tuple;
  }
  else if (nodeType == "Mapping")
  {
    return Mapping;
  }
  else if (nodeType == "FunctionCall")
  {
    if (expr["expression"]["nodeType"] == "NewExpression")
      return NewExpression;
    if (
      expr["expression"]["nodeType"] == "ElementaryTypeNameExpression" &&
      expr["kind"] == "typeConversion")
      return ElementaryTypeNameExpression;
    return CallExprClass;
  }
  else if (nodeType == "MemberAccess")
  {
    assert(expr.contains("expression"));
    ClarityGrammar::TypeNameT type_name =
      get_type_name_t(expr["expression"]["typeDescriptions"]);
    if (type_name == ClarityGrammar::TypeNameT::ContractTypeName)
      return ContractMemberCall;
    else
      //TODO Assume it's a builtin member
      // due to that the BuiltinTypeName cannot cover all the builtin member
      // e.g. string.concat ==> TypeConversionName
      return BuiltinMemberCall;
  }
  else if (nodeType == "ImplicitCastExprClass")
  {
    return ImplicitCastExprClass;
  }
  else if (nodeType == "IndexAccess")
  {
    return IndexAccess;
  }
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
  if (expr["operator"] == "--")
  {
    if (uo_pre)
      return UO_PreDec;
    else
      return UO_PostDec;
  }
  else if (expr["operator"] == "++")
  {
    if (uo_pre)
      return UO_PreInc;
    else
      return UO_PostInc;
  }
  else if (expr["operator"] == "-")
  {
    return UO_Minus;
  }
  else if (expr["operator"] == "~")
  {
    return UO_Not;
  }
  else if (expr["operator"] == "!")
  {
    return UO_LNot;
  }
  else
  {
    log_error(
      "Got expression operator={}. Unsupported expression operator",
      expr["operator"].get<std::string>());

    abort();
  }
}

ExpressionT get_expr_operator_t(const nlohmann::json &expr)
{
  if (expr["operator"] == "=")
  {
    return BO_Assign;
  }
  else if (expr["operator"] == "+")
  {
    return BO_Add;
  }
  else if (expr["operator"] == "-")
  {
    return BO_Sub;
  }
  else if (expr["operator"] == "*")
  {
    return BO_Mul;
  }
  else if (expr["operator"] == "/")
  {
    return BO_Div;
  }
  else if (expr["operator"] == "%")
  {
    return BO_Rem;
  }
  else if (expr["operator"] == "<<")
  {
    return BO_Shl;
  }
  else if (expr["operator"] == ">>")
  {
    return BO_Shr;
  }
  else if (expr["operator"] == "&")
  {
    return BO_And;
  }
  else if (expr["operator"] == "^")
  {
    return BO_Xor;
  }
  else if (expr["operator"] == "|")
  {
    return BO_Or;
  }
  else if (expr["operator"] == ">")
  {
    return BO_GT;
  }
  else if (expr["operator"] == "<")
  {
    return BO_LT;
  }
  else if (expr["operator"] == ">=")
  {
    return BO_GE;
  }
  else if (expr["operator"] == "<=")
  {
    return BO_LE;
  }
  else if (expr["operator"] == "!=")
  {
    return BO_NE;
  }
  else if (expr["operator"] == "==")
  {
    return BO_EQ;
  }
  else if (expr["operator"] == "&&")
  {
    return BO_LAnd;
  }
  else if (expr["operator"] == "||")
  {
    return BO_LOr;
  }
  else if (expr["operator"] == "+=")
  {
    return BO_AddAssign;
  }
  else if (expr["operator"] == "-=")
  {
    return BO_SubAssign;
  }
  else if (expr["operator"] == "*=")
  {
    return BO_MulAssign;
  }
  else if (expr["operator"] == "/=")
  {
    return BO_DivAssign;
  }
  else if (expr["operator"] == "%=")
  {
    return BO_RemAssign;
  }
  else if (expr["operator"] == "<<=")
  {
    return BO_ShlAssign;
  }
  else if (expr["operator"] == ">>=")
  {
    return BO_ShrAssign;
  }
  else if (expr["operator"] == "&=")
  {
    return BO_AndAssign;
  }
  else if (expr["operator"] == "^=")
  {
    return BO_XorAssign;
  }
  else if (expr["operator"] == "|=")
  {
    return BO_OrAssign;
  }
  else if (expr["operator"] == "**")
  {
    return BO_Pow;
  }
  else
  {
    log_error(
      "Got expression operator={}. Unsupported expression operator",
      expr["operator"].get<std::string>());
    abort();
  }

  return ExpressionTError; // make some old compilers happy
}

const char *expression_to_str(ExpressionT type)
{
  switch (type)
  {
    ENUM_TO_STR(BinaryOperatorClass)
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