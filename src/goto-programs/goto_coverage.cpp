#include <goto-programs/goto_coverage.h>

std::string goto_coveraget::get_filename_from_path(std::string path)
{
  if (path.find_last_of('/') != std::string::npos)
    return path.substr(path.find_last_of('/') + 1);

  return path;
}

void goto_coveraget::make_asserts_false()
{
  log_progress("Converting all assertions to false...");
  Forall_goto_functions (f_it, goto_functions)
    if (f_it->second.body_available && f_it->first != "__ESBMC_main")
    {
      goto_programt &goto_program = f_it->second.body;
      Forall_goto_program_instructions (it, goto_program)
      {
        const expr2tc old_guard = it->guard;
        if (it->is_assert())
        {
          it->guard = gen_false_expr();
          it->location.property("instrumented assertion");
          it->location.comment(from_expr(ns, "", old_guard));
          it->location.user_provided(true);
        }
      }
    }
}

void goto_coveraget::make_asserts_true()
{
  log_progress("Converting all assertions to true...");
  Forall_goto_functions (f_it, goto_functions)
    if (f_it->second.body_available && f_it->first != "__ESBMC_main")
    {
      goto_programt &goto_program = f_it->second.body;
      Forall_goto_program_instructions (it, goto_program)
      {
        const expr2tc old_guard = it->guard;
        if (it->is_assert())
        {
          it->guard = gen_true_expr();
          // not an instrumentation
          it->location.property("assertion");
          it->location.comment(from_expr(ns, "", old_guard));
          it->location.user_provided(true);
        }
      }
    }
}

void goto_coveraget::add_false_asserts()
{
  log_progress("Adding false assertions...");
  Forall_goto_functions (f_it, goto_functions)
    if (f_it->second.body_available && f_it->first != "__ESBMC_main")
    {
      goto_programt &goto_program = f_it->second.body;
      Forall_goto_program_instructions (it, goto_program)
      {
        if (it->is_end_function())
        {
          // insert an assert(0) as instrumentation BEFORE each instruction
          insert_assert(goto_program, it, gen_false_expr());
          continue;
        }

        if ((!is_true(it->guard) && it->is_goto()) || it->is_target())
        {
          it++; // add an assertion behind the instruciton
          insert_assert(goto_program, it, gen_false_expr());
          continue;
        }
      }

      goto_programt::targett it = goto_program.instructions.begin();
      insert_assert(goto_program, it, gen_false_expr());
    }
}

void goto_coveraget::insert_assert(
  goto_programt &goto_program,
  goto_programt::targett &it,
  const expr2tc &guard)
{
  insert_assert(goto_program, it, guard, from_expr(ns, "", guard));
}

void goto_coveraget::insert_assert(
  goto_programt &goto_program,
  goto_programt::targett &it,
  const expr2tc &guard,
  const std::string &idf)
{
  goto_programt::targett t = goto_program.insert(it);
  t->type = ASSERT;
  t->guard = guard;
  t->location = it->location;
  t->location.property("instrumented assertion");
  t->location.comment(idf);
  t->location.user_provided(true);
  it = ++t;
}

int goto_coveraget::get_total_instrument() const
{
  int total_instrument = 0;
  forall_goto_functions (f_it, goto_functions)
    if (f_it->second.body_available && f_it->first != "__ESBMC_main")
    {
      const goto_programt &goto_program = f_it->second.body;
      forall_goto_program_instructions (it, goto_program)
      {
        if (
          it->is_assert() &&
          it->location.property().as_string() == "instrumented assertion" &&
          it->location.user_provided() == true)
        {
          total_instrument++;
        }
      }
    }
  return total_instrument;
}

// Count the total assertion instances in goto level via goto-unwind api
// run the algorithm on the copy of the original goto program
int goto_coveraget::get_total_assert_instance() const
{
  // 1. execute goto uniwnd
  int total_assert_instance = 0;
  bounded_loop_unroller unwind_loops;
  unwind_loops.run(goto_functions);
  // 2. calculate the number of assertion instance
  forall_goto_functions (f_it, goto_functions)
    if (f_it->second.body_available && f_it->first != "__ESBMC_main")
    {
      const goto_programt &goto_program = f_it->second.body;
      forall_goto_program_instructions (it, goto_program)
      {
        if (
          it->is_assert() &&
          it->location.property().as_string() == "instrumented assertion" &&
          it->location.user_provided() == true)
          total_assert_instance++;
      }
    }
  return total_assert_instance;
}

std::unordered_set<std::string> goto_coveraget::get_total_cond_assert() const
{
  std::unordered_set<std::string> total_cond_assert = {};
  std::string idf = "";

  forall_goto_functions (f_it, goto_functions)
  {
    if (f_it->second.body_available && f_it->first != "__ESBMC_main")
    {
      const goto_programt &goto_program = f_it->second.body;
      forall_goto_program_instructions (it, goto_program)
      {
        if (
          it->is_assert() &&
          it->location.property().as_string() == "instrumented assertion" &&
          it->location.user_provided() == true)
        {
          idf = it->location.comment().as_string() + "\t" +
                it->location.as_string();
          total_cond_assert.insert(idf);
        }
      }
    }
  }
  return total_cond_assert;
}

/*
  Condition Coverage: fault injection
  1. find condition statements, this includes the converted for_loop/while
  2. insert assertion instances before that statement.
  e.g.
    if (a >1)
  =>
    assert(!(a>1))
    assert(a>1)
    if(a>1)
  then run multi-property
*/
void goto_coveraget::gen_cond_cov()
{
  Forall_goto_functions (f_it, goto_functions)
    if (f_it->second.body_available && f_it->first != "__ESBMC_main")
    {
      goto_programt &goto_program = f_it->second.body;
      Forall_goto_program_instructions (it, goto_program)
      {
        const std::string cur_filename =
          get_filename_from_path(it->location.file().as_string());
        filename = get_filename_from_path(filename);

        // e.g. IF !(a > 1) THEN GOTO 3
        if (!is_true(it->guard) && it->is_goto() && filename == cur_filename)
        {
          // e.g.
          //    GOTO 2;
          //    2: IF(...);
          if (it->is_target())
            target_num = it->target_number;

          // preprocessing: if(true) ==> if(true == true)
          exprt guard = migrate_expr_back(it->guard);
          guard = handle_single_guard(guard);

          // split the guard
          std::list<exprt> operands;
          std::list<std::string> operators;
          bool dump = false;
          collect_operands(guard, operands, dump);
          collect_operators(guard, operators);

          // e.g. if(a == 1)
          if (operands.empty())
            operands.push_back(guard);

          auto opd = operands.begin();
          auto opt = operators.begin();

          // fisrt atoms
          std::set<exprt> atoms;
          collect_atom_operands(*opd, atoms);
          if (atoms.empty())
          {
            log_error(
              "Internal error when collecting atom condition expression.");
            abort();
          }
          add_cond_cov_init_assert(*atoms.begin(), goto_program, it);

          // set up pointer to re-build the binary tree
          //   ||       <-- top_ptr
          // a    &&
          //     b   c   <--- rhs_ptr
          exprt root;
          root.operands().emplace_back(*opd);
          const exprt::operandst::iterator root_ptr = root.operands().begin();
          exprt::operandst::iterator top_ptr = root.operands().begin();
          exprt::operandst::iterator rhs_ptr = top_ptr;
          std::vector<exprt::operandst::iterator> top_ptr_stack;
          opd++;

          while (opd != operands.end() && opt != operators.end())
          {
            if (*opt == "(")
            {
              if (!top_ptr->is_empty())
              {
                // store
                top_ptr_stack.emplace_back(top_ptr);
                top_ptr = rhs_ptr;
              }
              ++opt;
              continue;
            }
            else if (*opt == ")")
            {
              if (!top_ptr_stack.empty())
              {
                // retrieve top_ptr
                top_ptr = top_ptr_stack.back();
                top_ptr_stack.pop_back();
              }
              ++opt;
              continue;
            }
            else if (*opt == "&&" || *opt == "||")
            {
              // get atom
              atoms = {};
              const exprt elem = *opd;
              collect_atom_operands(elem, atoms);

              assert(atoms.size() == 1);
              const auto &atom = *atoms.begin();
              irep_idt op = (*opt) == "&&" ? exprt::id_and : exprt::id_or;
              add_cond_cov_rhs_assert(
                op, top_ptr, rhs_ptr, root_ptr, atom, goto_program, it);
            }
            else
            {
              log_error("Unexpected operators");
              abort();
            }

            // update counter
            assert(opd != operands.end() && opt != operators.end());
            ++opd;
            ++opt;
          }
        }
        target_num = -1;
      }
    }
}

void goto_coveraget::add_cond_cov_init_assert(
  const exprt &expr,
  goto_programt &goto_program,
  goto_programt::targett &it)
{
  expr2tc guard;
  migrate_expr(expr, guard);

  // e.g. assert(!(a==1));  // a==1
  std::string idf = from_expr(ns, "", guard);
  make_not(guard);

  // insert assert
  insert_assert(goto_program, it, guard, idf);

  if (target_num != -1)
  {
    // update target
    std::vector<goto_programt::instructiont::targett> tgt_list;
    Forall_goto_program_instructions (itt, goto_program)
    {
      //! assume only one target
      if (
        itt->is_goto() && itt->has_target() &&
        itt->get_target()->target_number == (unsigned)target_num)
      {
        tgt_list.push_back(itt);
      }
    }

    if (!tgt_list.empty())
    {
      //! do not change the order
      // 1. rm original tgt_num
      it->target_number = -1;

      // 2. add tgt_num to the instrumentation  (x: ASSERT)
      --it;
      it->target_number = target_num;

      // 3. update src (GOTO x)
      for (auto &itt : tgt_list)
        itt->set_target(it);

      // 4. reset
      ++it;
    }
  }

  // reversal
  idf = from_expr(ns, "", guard);
  make_not(guard);
  insert_assert(goto_program, it, guard, idf);
}

/*
  algo:
  if(b==0 && c > 90)
  => assert(b==0)
  => assert(!(b==0));
  => assert(!(b==0 && c>90))
  => assert(!(b==0 && !(c>90)))

  if(b==0 || c > 90)
  => assert(b==0)
  => assert((b==0));
  => assert(!(!b==0 && c>90))
  => assert(!(!(b==0) && !(c>90)))
*/
void goto_coveraget::add_cond_cov_rhs_assert(
  const irep_idt &op_tp,
  exprt::operandst::iterator &top_ptr,
  exprt::operandst::iterator &rhs_ptr,
  const exprt::operandst::iterator &root_ptr,
  const exprt &rhs,
  goto_programt &goto_program,
  goto_programt::targett &it)
{
  /* 
  example:
     root
      ||  <- root_ptr/top_ptr
   &&   b <- rhs_ptr/top_ptr
  a  a  
  */

  // 0. store previous state
  const exprt old_top = *top_ptr;
  const exprt old_root = *root_ptr;

  // 2. build new joined expr
  exprt lhs_expr;
  if (op_tp == exprt::id_or)
  {
    exprt not_expr = exprt("not", bool_type());
    not_expr.operands().emplace_back(*top_ptr);
    lhs_expr = not_expr;
  }
  else
    lhs_expr = *top_ptr;

  exprt rhs_not_expr = exprt("not", bool_type());
  rhs_not_expr.operands().emplace_back(rhs);
  exprt join_expr = exprt(exprt::id_and, bool_type());
  join_expr.operands().emplace_back(lhs_expr);
  join_expr.operands().emplace_back(rhs_not_expr);

  // 2. replace top_expr with the joined expr
  // the rhs of (*root_ptr) is also changed during this process
  *top_ptr = join_expr;

  // 3. preprocess for pre_cond lhs
  bool pre_cond_flg = false;
  if (root_ptr->has_operands() && root_ptr->operands().size() == 2)
  {
    const std::string sub_id = (*root_ptr).op0().id().as_string();
    // pre_cond_flg: the lhs should be an atom.
    pre_cond_flg = (sub_id != "constant" && sub_id != "symbol") &&
                   (*root_ptr).op0().has_operands() &&
                   (*root_ptr).id() == exprt::id_or;
    if (pre_cond_flg)
    {
      // change 'a && a or b' to 'not (a && a) and b'
      exprt not_pre_cond = exprt("not", bool_type());
      not_pre_cond.operands().emplace_back(root_ptr->op0());
      root_ptr->op0() = not_pre_cond;
      root_ptr->id(exprt::id_and);
    }
  }

  exprt join_not_expr = exprt("not", bool_type());
  join_not_expr.operands().emplace_back(*root_ptr);

  // 4. obtain guard
  expr2tc guard;
  migrate_expr(join_not_expr, guard);
  expr2tc a_guard;
  migrate_expr(rhs, a_guard);
  make_not(a_guard);

  // 5. modified insert_assert
  std::string idf = from_expr(ns, "", a_guard);
  insert_assert(goto_program, it, guard, idf);

  // 6. reversal
  *root_ptr = old_root;
  *top_ptr = old_top;
  join_not_expr.clear();

  exprt join_rev_expr = exprt(exprt::id_and, bool_type());
  join_rev_expr.operands().emplace_back(lhs_expr);
  join_rev_expr.operands().emplace_back(rhs);

  *top_ptr = join_rev_expr;
  if (pre_cond_flg)
  {
    exprt not_pre_cond = exprt("not", bool_type());
    not_pre_cond.operands().emplace_back(root_ptr->op0());
    root_ptr->op0() = not_pre_cond;
    root_ptr->id(exprt::id_and);
  }

  join_not_expr = exprt("not", bool_type());
  join_not_expr.operands().emplace_back(*root_ptr);

  migrate_expr(join_not_expr, guard);
  migrate_expr(rhs, a_guard);

  idf = from_expr(ns, "", a_guard);
  insert_assert(goto_program, it, guard, idf);

  // 7. restore root_ptr
  // noted that this should be done before the updating of *top_ptr
  if (pre_cond_flg)
  {
    // change 'not (a && a) and b' back to 'a && a or b'
    *root_ptr = old_root;
  }

  // 8. update *top_ptr;
  join_expr.clear();
  *top_ptr = old_top;
  join_expr = exprt(op_tp, bool_type());
  join_expr.operands().emplace_back(*top_ptr);
  join_expr.operands().emplace_back(rhs);
  *top_ptr = join_expr;

  // 9. update rhs_ptr
  rhs_ptr = top_ptr->operands().begin();
  rhs_ptr++;
}

/*
  collect the operands splited by && and ||
  - flag: indicated the (sub-)expression have beed handled or not
*/
void goto_coveraget::collect_operands(
  const exprt &expr,
  std::list<exprt> &operands,
  bool &flag)
{
  const irep_idt &id = expr.id();

  if ((id == irept::id_and || id == irept::id_or) && flag == false)
  {
    bool flg0 = false;
    collect_operands(expr.op0(), operands, flg0);
    if (!flg0)
      operands.emplace_back(expr.op0());
    // operators.emplace_back(id);
    bool flg1 = false;
    collect_operands(expr.op1(), operands, flg1);
    if (!flg1)
      operands.emplace_back(expr.op1());
    flag = true;
  }
  else
  {
    forall_operands (it, expr)
      collect_operands(*it, operands, flag);
  }
}

/*
  collect the conditions (i.e. atom operands) identified by the relational operators
*/
void goto_coveraget::collect_atom_operands(
  const exprt &expr,
  std::set<exprt> &atoms)
{
  const auto &id = expr.id();
  forall_operands (it, expr)
    collect_atom_operands(*it, atoms);
  if (
    id == exprt::equality || id == exprt::notequal || id == exprt::i_lt ||
    id == exprt::i_gt || id == exprt::i_le || id == exprt::i_ge)
  {
    atoms.insert(expr);
  }
}

/*
  collect the operators from the expression
*/
void goto_coveraget::collect_operators(
  const exprt &expr,
  std::list<std::string> &operators)
{
  std::string str = from_expr(expr);
  // prepocess: remove parenthesis in type_cast
  // e.g. if(return_bool()) ==> IF !(_Bool)return_value$_return_bool$1
  //                        ==> IF !return_value$_return_bool$1
  // !FIXME: maybe this should be regex like \(\_[A-Za-z]+\) iF there are other typecasts
  const std::string subStr = "(_Bool)";
  size_t pos = str.find(subStr);
  while (pos != std::string::npos)
  {
    str.erase(pos, subStr.length());
    pos = str.find(subStr, pos);
  }

  std::list<std::string> opt;
  for (std::size_t i = 0; i < str.length(); i++)
  {
    if (str[i] == ' ')
      continue;
    else if (str[i] == '|' && i + 1 < str.length() && str[i + 1] == '|')
    {
      opt.emplace_back("||");
      i++;
    }
    else if (str[i] == '&' && i + 1 < str.length() && str[i + 1] == '&')
    {
      opt.emplace_back("&&");
      i++;
    }
    else if (str[i] == '(')
      opt.emplace_back("(");
    else if (str[i] == ')')
      opt.emplace_back(")");
  }

  // add implied parentheses in boolean expression
  // e.g.
  //   if(a&&b || c&&d) ==> if((a&&b) || (c&&d))
  //   (a == 1 || b != 2 && (b == 3 || a == 1)) => (a == 1 || (b != 2 && (b == 3 || a == 1)))
  // general rule: add parenthesis between || and &&
  // (&&||&&)
  std::list<std::string> tmp;
  std::string lst_op = "";
  std::vector<std::string> pnt_stk;
  for (auto &op : opt)
  {
    if (op == "(")
    {
      if (pnt_stk.empty() && !tmp.empty())
      {
        // combine
        operators.insert(operators.end(), tmp.begin(), tmp.end());
        tmp.clear();
      }
      pnt_stk.emplace_back("(");
    }
    else if (op == ")")
    {
      tmp.emplace_back(")");
      tmp.emplace_front("(");
      if (!pnt_stk.empty())
        pnt_stk.pop_back();

      if (pnt_stk.empty() && !tmp.empty())
      {
        operators.insert(operators.end(), tmp.begin(), tmp.end());
        tmp.clear();
      }
    }
    else if (op == "&&" && lst_op == "||")
    {
      if (!tmp.empty())
      {
        operators.insert(operators.end(), tmp.begin(), tmp.end());
        tmp.clear();
      }
      pnt_stk.push_back("(");
      tmp.emplace_back(op);
    }
    else if (op == "||" && lst_op == "&&")
    {
      tmp.emplace_front("(");
      tmp.emplace_back(")");
      if (!pnt_stk.empty())
        pnt_stk.pop_back();

      if (pnt_stk.empty() && !tmp.empty())
      {
        operators.insert(operators.end(), tmp.begin(), tmp.end());
        tmp.clear();
      }
      tmp.emplace_back(op);
    }
    else
    {
      tmp.emplace_back(op);
    }
    lst_op = op;
  }

  if (!tmp.empty())
  {
    while (!pnt_stk.empty())
    {
      tmp.emplace_back(")");
      tmp.emplace_front("(");
      pnt_stk.pop_back();
    }
    operators.insert(operators.end(), tmp.begin(), tmp.end());
  }
}

exprt goto_coveraget::gen_no_eq_expr(const exprt &lhs, const exprt &rhs)
{
  exprt not_eq_expr = exprt("notequal", bool_type());
  exprt new_expr = false_exprt();
  not_eq_expr.operands().emplace_back(lhs);
  not_eq_expr.operands().emplace_back(rhs);
  return not_eq_expr;
}

/*
  This function convert single guard to a non_equal_to_false expression
  e.g. if(true) ==> if(true!=false)
*/
exprt goto_coveraget::handle_single_guard(exprt &expr)
{
  if (expr.operands().size() == 0)
  {
    if (expr.id() == exprt::constant)
    {
      //    if (true) => if (true != false)
      exprt false_expr = false_exprt();
      return gen_no_eq_expr(expr, false_expr);
    }
    else
      // when expr.id() == exprt::symbol, there is always a upper node expr::typecast
      // thus we add no_equal_false at upper level, i.e. (bool)x ==> (bool)x != false
      return expr;
  }
  else if (expr.operands().size() == 1)
  {
    // Unary operator or typecast
    // e.g.
    //    if (!(bool)(a++)) => if(!(bool)(a++) != false)
    // note that we do not need to convert a++ to a++!=0

    if (expr.id() == exprt::typecast)
    {
      exprt false_expr = false_exprt();
      return gen_no_eq_expr(expr, false_expr);
    }
    else
    {
      Forall_operands (it, expr)
        *it = handle_single_guard(*it);
    }
  }
  else if (expr.operands().size() == 2)
  {
    if (expr.id() == exprt::id_and || expr.id() == exprt::id_or)
    {
      // e.g. if(a && b) ==> if(a!=0 && b!=0)
      Forall_operands (it, expr)
        *it = handle_single_guard(*it);
    }
    // "there always a typecast bool beforehand"
    // e.g. bool a[10]; if(a[1]) ==> if((bool)a[1])
    // thus we do not need to handle other 2-opd expression here
  }
  else if (expr.operands().size() == 3)
  {
    // if(a ? b:c) ==> if (a!=0 ? b!=0 : c!=0)
    Forall_operands (it, expr)
      *it = handle_single_guard(*it);
  }

  // fall through
  return expr;
}
