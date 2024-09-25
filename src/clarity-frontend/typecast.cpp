#include <clarity-frontend/typecast.h>
#include <util/c_typecast.h>
#include <util/c_types.h>
#include <util/simplify_expr_class.h>
#include <stdexcept>
#include <sstream>
#include <expr_util.h>
#include <util/std_code.h>

void clarity_gen_typecast(const namespacet &ns, exprt &dest, const typet &type)
{
  c_typecastt c_typecast(ns);
  c_typecast.implicit_typecast(dest, type);
}


void clarity_typecast_response(exprt &source_val, const typet &dest_type)
{
  //if (source_val.type().id() == typet::id_struct)    
  // [TODO]Temporarily disable function for struct conversion
  if (false)    
    {
      typet source_type;
      if (source_val.type().id() == typet::id_struct && source_val.type().get("#clar_type") == "response")
      {
        source_type = source_val.type(); 
      }
      else if (source_val.type().id() == typet::id_code && to_code_type(source_val.type()).return_type().get("#clar_type") == "response")
      {
        source_type = to_code_type(source_val.type()).return_type();
      }
      else
      {
        return;
      }

      if (!source_type.is_empty())
      {
        
        symbol_exprt src_struct("src_struct", source_type);
        code_assignt assign_result_src(src_struct, source_val);
        code_blockt _block;
        
        _block.operands().push_back(assign_result_src);

        irep_idt destination_struct_name = "dest_struct";
        symbol_exprt destination_struct(destination_struct_name, dest_type);  // Another variable of the same struct type
        code_assignt assign_result_dest(destination_struct, gen_zero(dest_type));
        _block.operands().push_back(assign_result_dest);

        
        // Step 2: Iterate through the struct_typet components and print their names
        int index = 0;
        for (const auto &comp : to_struct_type(source_type).components()) {
            // Get and print the name of the component
            std::string component_name = id2string(comp.get_name());
            //std::cout << "Component name: " << component_name << std::endl;
            if ((component_name == "is_ok") ||
                (component_name == "ok_val") ||
                (component_name == "err_val")
                )
            {
               member_exprt destinantion_field(destination_struct,  component_name, comp.type());
               member_exprt source_field(src_struct, component_name, comp.type());
               code_assignt assign_field(destinantion_field, source_field);  // Assign tmp.ok to destination_struct.ok
               _block.operands().push_back(assign_field);
            }
            
            index++;
        }

        symbol_exprt final_response_expr("stmt_expr_response", destination_struct.type());
        code_assignt assign_result(final_response_expr, destination_struct);
        _block.operands().push_back(assign_result);
      
        
        typet t = dest_type;
        side_effect_exprt stmt_expr("statement_expression", t);
        stmt_expr.copy_to_operands(_block);
        
        source_val.swap(stmt_expr);
        
        
        // source_val = inits;
      }
    }
}
