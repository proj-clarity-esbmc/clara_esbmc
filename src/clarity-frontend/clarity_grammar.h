#ifndef CLARITY_GRAMMAR_H_
#define CLARITY_GRAMMAR_H_

#include <map>
#include <string>
#include <nlohmann/json.hpp>

// Anything auxiliary means it's not in Clarity grammar, but we need it to work with
// ESBMC's irept
namespace ClarityGrammar
{
// rule contract-body-element
enum ContractBodyElementT
{
  VarDecl = 0, // rule variable-declaration
  FunctionDef, // rule function-definition
  ContractBodyElementTError
};
ContractBodyElementT get_contract_body_element_t(const nlohmann::json &element);
const char *contract_body_element_to_str(ContractBodyElementT type);

/* m-ali */
bool is_literal_type(std::string type);
bool is_state_variable(const nlohmann::json &ast_node);
bool is_variable_declaration(const nlohmann::json &ast_node);
bool is_function_definitionn(const nlohmann::json &ast_node);
bool is_tuple_declaration(const nlohmann::json &ast_node);
bool is_principal_declaration(const nlohmann::json &ast_node);
bool parse_expression_element(nlohmann::json &expr_element_json);
bool get_operation_type(nlohmann::json &expression_node);
bool operation_is_conditional(const nlohmann::json &ast_node);
bool operation_is_unary(const nlohmann::json &ast_node);
bool operation_is_binary(const nlohmann::json &ast_node);
bool operation_is_optional_decl(const nlohmann::json &ast_node);
nlohmann::json get_optional_type(const nlohmann::json &objtype);
std::string get_optional_symbolId(const nlohmann::json &optional_type);

/* end m-ali*/

// rule type-name
enum TypeNameT
{
  // rule elementary-type-name
  ElementaryTypeName = 0,

  // rule parameter-list. Strictly, this should not be here. Just a workaround
  ParameterList,

  // static array type
  ListTypeName,

  BuffTypeName,

  // contract type
  ContractTypeName,

  // Optional
  OptionalTypeName,

  // tuple
  TupleTypeName,

  // mapping
  MappingTypeName,

  // built-in member
  BuiltinTypeName,

  // return type
  ReturnTypeName,

  TypeNameTError
};
TypeNameT get_type_name_t(const nlohmann::json &type_name);
const char *type_name_to_str(TypeNameT type);

// rule elementary-type-name
enum ElementaryTypeNameT
{
  UINT,
  UINT_LITERAL,

  INT,
  INT_LITERAL,

  // rule bool
  BOOL,

  // rule address
  PRINCIPAL,

  // rule string
  // TODO: it's always (string-ascii <max-size>) or (string-utf8 <max-size>)
  STRING_ASCII,
  STRING_ASCII_LITERAL,

  STRING_UTF8,
  STRING_UTF8_LITERAL,

  // rule bytes
  // TODO: it's always (buff <some-size>)
  BUFF,
  BUFF_LITERAL,

  // error: (err ...)
  ERROR,

  // TODO: tuple: like a map
  // TODO: optional of any of the above
  // TODO: response of ok-type, err-type.

  ElementaryTypeNameTError
};
ElementaryTypeNameT get_elementary_type_name_t(const nlohmann::json &type_name);
const char *elementary_type_name_to_str(ElementaryTypeNameT type);
unsigned int uint_type_name_to_size(ElementaryTypeNameT);

unsigned int uint_type_name_to_size(ElementaryTypeNameT);
unsigned int int_type_name_to_size(ElementaryTypeNameT);
unsigned int bytesn_type_name_to_size(ElementaryTypeNameT);

// rule parameter-list
enum ParameterListT
{
  EMPTY = 0, // In Clarity, "void" means an empty parameter list
  ONE_PARAM,
  MORE_THAN_ONE_PARAM,
  ParameterListTError
};
ParameterListT get_parameter_list_t(const nlohmann::json &type_name);
const char *parameter_list_to_str(ParameterListT type);

// rule block
enum BlockT
{
  Statement = 0,
  BlockForStatement,
  BlockIfStatement,
  BlockExpressionStatement,
  UncheckedBlock,
  BlockTError
};
BlockT get_block_t(const nlohmann::json &block);
const char *block_to_str(BlockT type);

// rule block
enum FuncBlockT
{
  SingleStatement = 0,
  SingleObject,
  MultipleStatement,
  FuncBlockTError
};
FuncBlockT get_function_block_t(const nlohmann::json &block);
const char *function_block_to_str(FuncBlockT type);

// rule statement
enum StatementT
{
  Block = 0,             // rule block (mutual inclusion)
  ExpressionStatement,   // rule expression-statement
  VariableDeclStatement, // rule variable-declaration-statement
  ReturnStatement,       // rule return-statement
  ForStatement,          // rule for-statement
  IfStatement,           // rule if-statement
  StatementTError,
  ContinueStatement, // rule continue
  BreakStatement,    // rule break
  RevertStatement    // rule revert
};
StatementT get_statement_t(const nlohmann::json &stmt);
const char *statement_to_str(StatementT type);

// rule expression-statement
//  - Skipped since it just contains 1 type: "expression + ;"

// rule expression
// these are used to identify the type of the expression
enum ExpressionT
{
  // BinaryOperator
  BinaryOperatorClass =
    0, // This type covers all binary operators in Clarity, such as =, +, - .etc
  BO_Assign, // =
  BO_Add,    // +
  BO_Sub,    // -
  BO_Mul,    // *
  BO_Div,    // /
  BO_Rem,    // %

  BO_Shl, // <<
  BO_Shr, // >>
  BO_And, // &
  BO_Xor, // ^
  BO_Or,  // |

  BO_GT,   // >
  BO_LT,   // <
  BO_GE,   // >=
  BO_LE,   // <=
  BO_NE,   // !=
  BO_EQ,   // ==
  BO_LAnd, // &&
  BO_LOr,  // ||

  BO_AddAssign, // +=
  BO_SubAssign, // -=
  BO_MulAssign, // *=
  BO_DivAssign, // /=
  BO_RemAssign, // %=
  BO_ShlAssign, // <<=
  BO_ShrAssign, // >>=
  BO_AndAssign, // &=
  BO_XorAssign, // ^=
  BO_OrAssign,  // |=
  BO_Pow,       // **

  // UnaryOperator
  UnaryOperatorClass,
  UO_PreDec,  // --
  UO_PreInc,  // ++
  UO_PostDec, // --
  UO_PostInc, // ++
  UO_Minus,   // -
  UO_Not,     // ~
  UO_LNot,    // !

  //TenaryOperator
  ConditionalOperatorClass, // ?...:...

  // rule identifier
  DeclRefExprClass,

  // rule literal
  Literal,

  // rule Tuple
  Tuple,

  //rule optional
  Optional,

  // rule list
  List,

  // rule Mapping
  Mapping,

  // FunctionCall
  CallExprClass,

  // auxiliary type for implicit casting in Clarity, e.g. function return value
  // Clarity does NOT provide such information.
  ImplicitCastExprClass,

  // auxiliary type for array's "[]" operator
  // equivalent to clang::Stmt::ArraySubscriptExprClass
  // Clarity does NOT provide such rule
  IndexAccess,

  // Create a temporary object by keywords 'ew'
  // equivalent to clang::Stmt::CXXTemporaryObjectExprClass
  // i.e. Base x = new Base(args);
  NewExpression,

  // Call member functions
  // equivalent toclang::Stmt::CXXMemberCallExprClass
  // i.e. x.caller();
  ContractMemberCall,

  // Type Converion
  ElementaryTypeNameExpression,

  // Struct Member Access
  StructMemberCall,

  // Enum Member Access
  EnumMemberCall,

  // Built-in Member Access
  BuiltinMemberCall,

  // Null Expression
  NullExpr,

  ExpressionTError
};
ExpressionT get_expression_t(const nlohmann::json &expr);
ExpressionT get_expr_operator_t(const nlohmann::json &expr);
ExpressionT
get_unary_expr_operator_t(const nlohmann::json &expr, bool uo_pre = true);
const char *expression_to_str(ExpressionT type);

// rule variable-declaration-statement
enum VarDeclStmtT
{
  VariableDecl,      // rule variable-declaration
  VariableDeclTuple, // rule variable-declaration-tuple
  VarDeclStmtTError
};
VarDeclStmtT get_var_decl_stmt_t(const nlohmann::json &stmt);
const char *var_decl_statement_to_str(VarDeclStmtT type);

// auxiliary type to convert function call
// No corresponding Clarity rules
enum FunctionDeclRefT
{
  FunctionProto = 0,
  FunctionNoProto,
  FunctionDeclRefTError
};
FunctionDeclRefT get_func_decl_ref_t(const nlohmann::json &decl);
const char *func_decl_ref_to_str(FunctionDeclRefT type);

// auxiliary type for implicit casting
enum ImplicitCastTypeT
{
  // for return value casting
  LValueToRValue = 0,

  // for ImplicitCastExpr<FunctionToPointerDecay> as in CallExpr when making a function call
  FunctionToPointerDecay,

  // for ImplicitCastExpr<ArrayToPointerDecay> as in IndexAccess
  ArrayToPointerDecay,

  ImplicitCastTypeTError
};
ImplicitCastTypeT get_implicit_cast_type_t(std::string cast);
const char *implicit_cast_type_to_str(ImplicitCastTypeT type);

// the function visibility
enum VisibilityT
{
  // any contract and account can call
  PublicT,

  // only inside the contract that defines the function
  PrivateT,
};
VisibilityT get_access_t(const nlohmann::json &ast_node);


bool get_literal_type_from_expr(const nlohmann::json &expr, nlohmann::json &expression_node);
}; // namespace ClarityGrammar

#endif /* CLARITY_GRAMMAR_H_ */
