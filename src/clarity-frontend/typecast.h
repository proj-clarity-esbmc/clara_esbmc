#ifndef CLARITY_TYPECAST_H_
#define CLARITY_TYPECAST_H_

#include <util/namespace.h>
#include <util/std_expr.h>

extern void
clarity_gen_typecast(const namespacet &ns, exprt &dest, const typet &type);
extern void clarity_typecast_response(
  contextt &context,
  exprt &source_val,
  const typet &dest_type);

#endif /* CLARITY_TYPECAST_H_ */
