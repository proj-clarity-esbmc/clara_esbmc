#include <clarity-frontend/typecast.h>
#include <util/c_typecast.h>
#include <util/c_types.h>
#include <util/message.h>
#include <util/expr_util.h>
#include <util/simplify_expr_class.h>
#include <stdexcept>
#include <sstream>

void clarity_gen_typecast(const namespacet &ns, exprt &dest, const typet &type)
{
  c_typecastt c_typecast(ns);
  c_typecast.implicit_typecast(dest, type);
}
/**
 * add missing ok / err value to struct
 * set type as destination type and init with zero value
 */
void clarity_typecast_response(
  contextt &context,
  exprt &source_val,
  const typet &dest_type)
{
  typet source_type;
  if (
    source_val.type().id() == typet::id_struct &&
    source_val.type().get("#clar_type") == "response")
  {
    source_type = source_val.type();
  }
  else if (
    source_val.type().id() == typet::id_code &&
    to_code_type(source_val.type()).return_type().get("#clar_type") ==
      "response")
  {
    source_type = to_code_type(source_val.type()).return_type();
  }
  else
  {
    return;
  }

  if (!source_type.is_empty())
  {
    // / List of required components
    std::vector<std::string> required_components = {
      "is_ok", "ok_val", "err_val"};

    // Create a set to store the names of components in type & value
    std::set<std::string> type_components;
    std::set<std::string> value_components;

    // verify destination type is response struct type
    const struct_typet &dest_struct_type = to_struct_type(dest_type);
    for (const auto &comp : dest_struct_type.components())
    {
      std::string component_name = id2string(comp.get_name());
      type_components.insert(component_name);
    }

    for (const std::string &required_comp : required_components)
    {
      if (type_components.find(required_comp) == type_components.end())
      {
        log_error(
          "Error: Invalid response type. Missing '{}' component. Aborting...",
          std::string(required_comp));
        abort();
      }
    }

    // Iterate over the components and collect their names
    for (const auto &comp : to_struct_type(source_type).components())
    {
      std::string component_name = id2string(comp.get_name());
      value_components.insert(component_name);
    }

    for (const std::string &required_comp : required_components)
    {
      if (value_components.find(required_comp) == value_components.end())
      {
        if (required_comp == "is_ok")
        {
          log_error(
            "Error: 'is_ok' component is missing from source_val. aborting...");
          abort();
        }
        else if (required_comp == "ok_val" || required_comp == "err_val")
        {
          const struct_union_typet::componentt &missing_comp =
            dest_struct_type.get_component(required_comp);
          const exprt missing_val = gen_zero(missing_comp.type());
          // push missing type & value to source
          to_struct_type(source_val.type())
            .components()
            .push_back(missing_comp);
          source_val.copy_to_operands(missing_val);

          // the added struct symbol also won't have this component.
          // get the struct symbol name
          const std::string name =
            "tag-struct response_" +
            source_val.type().get("#clar_response_id").as_string();
          symbolt *type_symbol = context.find_symbol(name);
          if (type_symbol != nullptr)
          {
            to_struct_type(type_symbol->type)
              .components()
              .push_back(missing_comp);
          }
        }
      }
    }
  }
}
