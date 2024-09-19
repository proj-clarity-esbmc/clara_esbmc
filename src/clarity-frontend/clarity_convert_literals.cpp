#include <clarity-frontend/clarity_convert.h>
#include <util/arith_tools.h>
#include <util/bitvector.h>
#include <util/c_types.h>
#include <util/expr_util.h>
#include <util/ieee_float.h>
#include <util/string_constant.h>
#include <util/std_expr.h>

// Integer literal
// first argument : integer_literal is not used in Clarity frontend, but left for compatibility with other frontends
bool clarity_convertert::convert_integer_literal(
  const nlohmann::json &integer_literal,
  std::string the_value,
  exprt &dest)
{
  // clarity only supports 128 bit signed integers
  typet type = signedbv_typet(128);

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
  std::string the_value,
  exprt &dest)
{
  size_t string_size = the_value.size() + 1; //to accommodate \0
  typet type = array_typet(
    signed_char_type(),
    constant_exprt(
      integer2binary(string_size, bv_width(int_type())),
      integer2string(string_size),
      int_type()));
  // ToDo : Handle null terminator byte --> completed by catenating '\0' to the string
  string_constantt string(the_value + '\0', type, string_constantt::k_default);
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
      expression_node =
        nlohmann::json::array({"string-utf8", "string-utf8", "16"});
    }
    else if (buffer_type == "STRING_ASCII")
    {
      auto width = type.width().as_string();
      expression_node =
        nlohmann::json::array({"string-ascii", "string-ascii", width});
    }
  }
  else if (type.id() == typet::t_struct)
  {
    auto struct_type = type.get("#clar_type").as_string();
    if (struct_type == "response")
    {
      nlohmann::json ok_val_json;
      nlohmann::json err_val_json;
      // get the ok_type if it exists
      for (const auto &comp : to_struct_type(type).components()) 
      {
        std::string component_name = id2string(comp.get_name());
        if (component_name == "ok_val")
        {
          if (get_literal_type_from_typet(comp.type(), ok_val_json)) 
          {
            return true;
          }
        }
        if (component_name == "err_val")
        {
          if (get_literal_type_from_typet(comp.type(), err_val_json)) 
          {
            return true;
          }
        }
                
      }

      if (ok_val_json.empty())
      {
        ok_val_json = nlohmann::json::array({"none", "none", "1"});
      }
      if (err_val_json.empty())
      {
        err_val_json = nlohmann::json::array({"none", "none", "1"});
      }
      nlohmann::json ok_err_array = nlohmann::json::array();
      ok_err_array.push_back(ok_val_json);
      ok_err_array.push_back(err_val_json);
        
      expression_node = nlohmann::json::array();
      expression_node.push_back("response");
      expression_node.push_back("response");
      expression_node.push_back("2");
      expression_node.push_back(ok_err_array);
      
    }
  }
  else
  {
    return true; // unexpected
  }

  return false;
}
