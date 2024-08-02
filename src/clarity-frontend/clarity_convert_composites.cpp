#include <clarity-frontend/clarity_convert.h>
#include <util/arith_tools.h>
#include <util/bitvector.h>
#include <util/c_types.h>
#include <util/expr_util.h>
#include <util/ieee_float.h>
#include <util/string_constant.h>
#include <util/std_expr.h>

// Composite data types
/**
 * struct principal
  {
    bool contract_is_principal;
    bool contract_is_standard;      //if contract_is_principal is true, then contract_is_standard will be false
    char contract_name[128]; //128 bytes long contract name
    char issuer_principal_bytes[20];
    char version;
    char issuer_principal_str[41];
  
  }
 * @n: the bit width, default 256 (unsignedbv_typet(256))
*/
bool clarity_convertert::define_principal_struct()

{
 struct_typet t = struct_typet();

  // get name/id:
  std::string name, id;
  name = "principal";
  id = prefix + "struct " + name;

  // get type:
  t.tag("struct " + name);

  // get location
  locationt location_begin;
  location_begin.set_line(0);
  location_begin.set_file(absolute_path);

  // get debug module name
  std::string debug_modulename =
    get_modulename_from_path(location_begin.file().as_string());
  current_fileName = debug_modulename;

  // populate struct type symbol
  symbolt symbol;
  get_default_symbol(symbol, debug_modulename, t, name, id, location_begin);
  symbolt &added_symbol = *move_symbol_to_context(symbol);


/*
struct principal
{
    bool contract_is_principal;
    bool contract_is_standard;
    char contract_name[128]; //128 bytes long contract name
    char issuer_principal_bytes[20];
    char version;
    char issuer_principal_str[41];
  
  }
*/
std::unordered_map<std::string, nlohmann::json> principal_struct_members = {
    {"contract_is_principal", {{"bool", "bool", 1}}},
    {"contract_is_standard", {{ "bool", "bool", 1}}},
    {"contract_name", {{ "string-ascii", "string-ascii", 128}}},
    {"issuer_principal_bytes", {{ "string-utf8", "string-utf8", 20}}},
    {"version", {{ "string-utf8", "string-utf8", 1}}},
    {"issuer_principal_str", {{ "string-ascii", "string-ascii", 41}}}
  };
  for (auto& [key, value]: principal_struct_members)
  {
    struct_typet::componentt comp;

    // manually create a member_name
    const std::string mem_name = key;//it.key();
    const std::string mem_id = "clar:@C@" + current_contractName + "@" + name +
                               "@" + mem_name;

    // get type
    typet mem_type;
    nlohmann::json objtype = {value[1],value[1],1};
    
    if (get_type_description(objtype, mem_type))
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

    
  }

  t.location() = location_begin;
  added_symbol.type = t;

  return false;
}