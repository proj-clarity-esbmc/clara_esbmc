#include <clarity-frontend/clarity_convert.h>
#include <boost/algorithm/string.hpp>
#include <util/arith_tools.h>
#include <util/bitvector.h>
#include <util/c_types.h>
#include <util/expr_util.h>
#include <util/ieee_float.h>
#include <util/string_constant.h>
#include <util/std_expr.h>
#include <iostream>

// Integer literal
// first argument : integer_literal is not used in Clarity frontend, but left for compatibility with other frontends
bool clarity_convertert::convert_integer_literal(
  const nlohmann::json &integer_literal,
  std::string the_value,
  exprt &dest)
{
  // clarity only supports 128 bit signed integers
  typet type = signedbv_typet(128);
  type.set("#clar_type", "int");
  type.set("#clar_lit_type", "INT");

  exprt the_val;
  // extract the value: signed
  BigInt z_ext_value = string2integer(the_value);
  the_val = constant_exprt(
    integer2binary(z_ext_value, bv_width(type)),
    integer2string(z_ext_value),
    type);

  dest.swap(the_val);
  return false;
}

bool clarity_convertert::convert_unsigned_integer_literal(
  const nlohmann::json &unsigned_integer_literal,
  std::string the_value,
  exprt &dest)
{
  // clarity only supports 128 bit unsigned integers
  typet type = unsignedbv_typet(128);
  type.set("#clar_type", "uint");
  type.set("#clar_lit_type", "UINT");

  exprt the_val;
  // extract the value: signed
  BigInt z_ext_value = string2integer(the_value);
  the_val = constant_exprt(
    integer2binary(z_ext_value, bv_width(type)),
    integer2string(z_ext_value),
    type);

  dest.swap(the_val);
  return false;
}

// can probably be ignored.
// use convert_integer_literal instead.
bool clarity_convertert::convert_integer_literal_with_type(
  typet &type,
  std::string the_value,
  exprt &dest)
{
  if (type != signedbv_typet(128))
  {
    //std::cout <<"invalid type provided. "<<"Expected "<<"signedbv_typet \n";
    abort();
  }
  exprt the_val;
  // extract the value: unsigned
  BigInt z_ext_value = string2integer(the_value);
  the_val = constant_exprt(
    integer2binary(z_ext_value, bv_width(type)),
    integer2string(z_ext_value),
    type);

  dest.swap(the_val);
  return false;
}

bool clarity_convertert::convert_unsigned_integer_literal_with_type(
  typet &type,
  std::string the_value,
  exprt &dest)
{
  if (type != unsignedbv_typet(128))
  {
    //std::cout <<"invalid type provided. "<<"Expected "<<"unsignedbv_typet \n";
    abort();
  }

  exprt the_val;
  // extract the value: unsigned
  BigInt z_ext_value = string2integer(the_value);
  the_val = constant_exprt(
    integer2binary(z_ext_value, bv_width(type)),
    integer2string(z_ext_value),
    type);

  dest.swap(the_val);
  return false;
}

bool clarity_convertert::convert_bool_literal(
  const nlohmann::json &bool_literal,
  std::string the_value,
  exprt &dest)
{
  // typet type = bool_typet();
  // assert(type.is_bool());

  if (the_value == "true")
  {
    dest = true_exprt();
    return false;
  }

  if (the_value == "false")
  {
    dest = false_exprt();
    return false;
  }

  return true;
}

// Helper function to convert a Unicode code point to UTF-8
std::string unicode_to_utf8(uint32_t codepoint)
{
  std::string result;
  if (codepoint <= 0x7F)
  {
    // 1-byte sequence
    result.push_back(static_cast<char>(codepoint));
  }
  else if (codepoint <= 0x7FF)
  {
    // 2-byte sequence
    result.push_back(static_cast<char>(0xC0 | ((codepoint >> 6) & 0x1F)));
    result.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
  }
  else if (codepoint <= 0xFFFF)
  {
    // 3-byte sequence
    result.push_back(static_cast<char>(0xE0 | ((codepoint >> 12) & 0x0F)));
    result.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
    result.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
  }
  else if (codepoint <= 0x10FFFF)
  {
    // 4-byte sequence
    result.push_back(static_cast<char>(0xF0 | ((codepoint >> 18) & 0x07)));
    result.push_back(static_cast<char>(0x80 | ((codepoint >> 12) & 0x3F)));
    result.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
    result.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
  }
  return result;
}

// Function to convert string with \u{...} to UTF-8
std::string convert_to_utf8(const std::string &input)
{
  std::ostringstream output;
  size_t i = 0;

  while (i < input.size())
  {
    if (input[i] == '\\' && input[i + 1] == 'u' && input[i + 2] == '{')
    {
      // Found a \u{...} escape sequence, process it
      i += 3; // Skip \u{
      std::string unicode_str;
      while (i < input.size() && input[i] != '}')
      {
        unicode_str += input[i++];
      }
      i++; // Skip closing '}'

      uint32_t codepoint = std::stoul(unicode_str, nullptr, 16);
      output << unicode_to_utf8(codepoint);
    }
    else
    {
      // Regular character, just copy it
      output << input[i++];
    }
  }

  return output.str();
}

// TODO: Character literal
/**
 * @brief Converts the string literal to a string_constt
 * 
 * @param the_value the value of the literal
 * @param dest return reference
 * @return true Only if the function fails
 * @return false Only if the function successfully converts the literal
 */
bool clarity_convertert::convert_string_literal(
  const nlohmann::json &string_literal_type,
  std::string the_value,
  exprt &dest)
{
  ClarityGrammar::ElementaryTypeNameT elementary_type =
    ClarityGrammar::get_elementary_type_name_t(string_literal_type);

  if (
    elementary_type != ClarityGrammar::ElementaryTypeNameT::STRING_ASCII &&
    elementary_type !=
      ClarityGrammar::ElementaryTypeNameT::STRING_ASCII_LITERAL &&
    elementary_type != ClarityGrammar::ElementaryTypeNameT::STRING_UTF8 &&
    elementary_type != ClarityGrammar::ElementaryTypeNameT::STRING_UTF8_LITERAL)
  {
    // abort -- not a string literal
    std::cout << "Not a string" << std::endl;
    return true;
  }

  // No need to add extra null terminator, C-style string literals already include it
  size_t string_size =
    the_value.size() + 1; // +1 for the implicit null terminator

  // Create the array type
  typet type = array_typet(
    char_type(), // Use char_type() instead of signed_char_type()
    from_integer(string_size, size_type()));

  // ascii and utf8 strings
  const bool is_utf8 =
    ((elementary_type == ClarityGrammar::ElementaryTypeNameT::STRING_UTF8) ||
     (elementary_type ==
      ClarityGrammar::ElementaryTypeNameT::STRING_UTF8_LITERAL))
      ? true
      : false;
  std::string encoding = is_utf8 ? "string_utf8" : "string_ascii";
  const irep_idt kind =
    is_utf8 ? string_constantt::k_unicode : string_constantt::k_default;
  // convert to utf8
  std::string value = is_utf8 ? convert_to_utf8(the_value) : the_value;

  // set custom attributes
  type.set("#clar_type", encoding);
  type.set("#clar_lit_type", boost::algorithm::to_upper_copy(encoding));

  // Create the string constant
  string_constantt string(value, type, kind);

  dest.swap(string);
  return false;
}

/**
 * convert hex-string to uint constant
 * @n: the bit width, default 256 (unsignedbv_typet(256))
*/
bool clarity_convertert::convert_hex_literal(
  std::string the_value,
  exprt &dest,
  const int n)
{
  // remove "0x" prefix
  if (the_value.length() >= 2)
    if (the_value.substr(0, 2) == "0x")
    {
      the_value.erase(0, 2);
    }

  typet type;
  type = unsignedbv_typet(n);

  // e.g. 0x1f9840a85d5aF5bf1D1762F925BDADdC4201F984
  BigInt hex_addr = string2integer(the_value, 16);
  exprt the_val;
  the_val = constant_exprt(
    integer2binary(hex_addr, bv_width(type)), integer2string(hex_addr), type);

  dest.swap(the_val);
  return false;
}

/**
 * convert str-string to uint constant
 * @n: the bit width, default 256 (unsignedbv_typet(256))
*/
bool clarity_convertert::convert_uint_literal(
  const nlohmann::json &uint_literal,
  std::string the_value,
  exprt &dest)

{
  // remove "u" prefix
  if (the_value.length() >= 2)
    if (the_value.substr(0, 1) == "u")
    {
      the_value.erase(0, 1);
    }
  convert_unsigned_integer_literal(uint_literal, the_value, dest);
  return false;
}

// TODO: Float literal.
//    - Note: Currently clarity does NOT support floating point data types or fp arithmetic.
//      Everything is done in fixed-point arithmetic as of clarity compiler v0.8.6.

// input    : The typet from which the objtype has to be created
// output   : The objtype json from the functions type
// returns  : false if succesful, or true if failed.
//
// This function assumes that for the array types there
// will be a clar_lit_type field that describes which
// subtype of an array it is
bool clarity_convertert::get_literal_type_from_typet(
  const typet &type,
  nlohmann::json &expression_node)
{
  if (type.id() == typet::t_unsignedbv)
  {
    auto width = type.width().as_string();
    expression_node = nlohmann::json::array({"uint", "uint_" + width, width});
  }
  else if (type.id() == typet::t_signedbv)
  {
    auto width = type.width().as_string();
    expression_node = nlohmann::json::array({"int", "int_" + width, width});
  }
  else if (type.id() == typet::t_bool)
  {
    auto width = type.width().as_string();
    expression_node = nlohmann::json::array({"bool", "bool", "1"});
  }
  else if (type.id() == typet::t_array)
  {
    auto buffer_type = type.get("#clar_lit_type").as_string();
    if (buffer_type == "BUFF")
    {
      expression_node = nlohmann::json::array({"buffer", "buffer", "4"});
    }
    else if (buffer_type == "STRING_UTF8")
    {
      auto width = type.width().as_string();
      expression_node =
        nlohmann::json::array({"string-utf8", "string-utf8", width});
    }
    else if (buffer_type == "STRING_ASCII")
    {
      auto width = type.width().as_string();
      expression_node =
        nlohmann::json::array({"string-ascii", "string-ascii", width});
    }
  }
  else if (type.id() == typet::t_symbol)
  {
    // check if type name contains "map"
    if (type.get("#clar_type").as_string() == "mapping")
    {
      expression_node = nlohmann::json::array({"map", "map", "1"});
    }
  }
  else if (type.id() == typet::t_struct)
  {
    // check if type name contains "map"
    if (type.get("#clar_type").as_string() == "principal")
    {
      expression_node = nlohmann::json::array({"principal", "principal", "1"});
    }
    else if (type.get("#clar_type").as_string() == "list_bool")
    {
      auto list_size = type.get("#clar_list_size").as_string();
      expression_node = nlohmann::json::array(
        {"list", "list", list_size, {"bool", "bool", "1"}});
    }
    else if (type.get("#clar_type").as_string() == "list_uint128_t")
    {
      auto list_size = type.get("#clar_list_size").as_string();
      expression_node = nlohmann::json::array(
        {"list", "list", list_size, {"uint", "uint128_t", "128"}});
    }
    else if (type.get("#clar_type").as_string() == "list_int128_t")
    {
      auto list_size = type.get("#clar_list_size").as_string();
      expression_node = nlohmann::json::array(
        {"list", "list", list_size, {"int", "int128_t", "128"}});
    }
    else if (type.get("#clar_type").as_string() == "list_principal")
    {
      auto list_size = type.get("#clar_list_size").as_string();
      expression_node = nlohmann::json::array(
        {"list", "list", list_size, {"principal", "principal", "149"}});
    }
    else
    {
      return true;
    }
  }
  else
  {
    return true; // unexpected
  }

  return false;
}
