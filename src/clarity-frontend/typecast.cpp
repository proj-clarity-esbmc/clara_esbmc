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
      
      if (source_val.type().get("#clar_type") == "response")
      {
        exprt inits;
        inits = gen_zero(dest_type);

        exprt::operandst val_operands = source_val.operands();
        
        symbol_exprt result_expr("src_struct", source_val.type());
        code_assignt assign_result_src(result_expr, source_val);
        code_blockt _block;
        code_declt decl_tmp(result_expr);
        _block.operands().push_back(decl_tmp);
        _block.operands().push_back(assign_result_src);

        irep_idt another_struct_name = "dest_struct";
        symbol_exprt another_struct(another_struct_name, dest_type);  // Another variable of the same struct type
        //exprt another_struct = gen_zero(dest_type);
        code_declt decl(another_struct);
        _block.operands().push_back(decl);
        // inits.operands().at(0) = val_operands.at(0);
        // member_exprt(val, "ok_val", signedbv_typet(32)));
        // Step 2: Iterate through the struct_typet components and print their names
        int index = 0;
        for (const auto &comp : to_struct_type(source_val.type()).components()) {
            // Get and print the name of the component
            std::string component_name = id2string(comp.get_name());
            //std::cout << "Component name: " << component_name << std::endl;
            if (component_name == "is_ok")
            {
              member_exprt another_ok(another_struct,  "is_ok", comp.type());
              member_exprt tmp_ok(result_expr, "is_ok", comp.type());
              code_assignt assign_another_ok(another_ok, tmp_ok);  // Assign tmp.ok to another_struct.ok
              _block.operands().push_back(assign_another_ok);
              //inits.operands().at(0) = val_operands.at(index);//member_exprt(result_expr, "is_ok", comp.type());
            }
            if (component_name == "ok_val")
            {
              member_exprt another_ok_val(another_struct, "ok_val", comp.type());
              member_exprt tmp_ok(result_expr, "ok_val", comp.type());
              code_assignt assign_another_ok_val(another_ok_val, tmp_ok);  // Assign tmp.ok to another_struct.ok
              _block.operands().push_back(assign_another_ok_val);
              
              //inits.operands().at(1) = val_operands.at(index);//member_exprt(result_expr, "ok_val", comp.type());
            }
            if (component_name == "err_val")
            {
              member_exprt another_err_val(another_struct, "err_val", comp.type());
              member_exprt tmp_ok(result_expr, "err_val", comp.type());
              code_assignt assign_another_err_val(another_err_val, tmp_ok);  // Assign tmp.ok to another_struct.ok
              _block.operands().push_back(assign_another_err_val);
              //inits.operands().at(2) = val_operands.at(index);//member_exprt(result_expr, "ok_val", comp.type());
            }
            
            index++;
        }
        // code_blockt _block;
        // _block.operands().push_back(assign_result);
        // _block.operands().push_back(inits);
        // typet t = dest_type;
        // side_effect_exprt stmt_expr("statement_expression", t);
        // stmt_expr.copy_to_operands(_block);
        // source_val = stmt_expr;
        // //source_val = assign_result;
        symbol_exprt final_result_expr("final_result", another_struct.type());
        code_assignt assign_result(final_result_expr, another_struct);
        _block.operands().push_back(assign_result);
      
        
        //_block.operands().push_back(another_struct);
        typet t = dest_type;
        side_effect_exprt stmt_expr("statement_expression", t);
        stmt_expr.copy_to_operands(_block);
        source_val = stmt_expr;
        
        
        // source_val = inits;
      }
    }
}
