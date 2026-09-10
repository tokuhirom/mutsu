use Test;

# An expression-position `given` with a matching `when`/`default` runs the
# body to completion via the implicit `succeed` control signal
# (`exec_do_given_expr_op`'s `is_succeed` branch in
# src/vm/vm_given_when_ops.rs). That branch used to omit the
# `self.stack.truncate(stack_base)` call its sibling `Ok` branch and the
# statement-form twin `exec_given_op` both already have, so the matched
# body's value was left on the stack *and* carried again via the succeed
# signal's `return_value` -- a net +2 push instead of +1. Any call whose
# argument list contained an expression-position `given` was affected: the
# extra value shifted every later argument down by one slot, silently
# eating a sibling argument (see todo/tickets/given-expr-succeed-branch-leaks-body-stack-value.md).

plan 7;

is (given 2 { when 2 { "two" } }), "two",
    'bare expr-position given/when yields exactly one value';

my @got;
@got.push("A: ", (given 2 { when 2 { "two" } }));
is @got.elems, 2, 'given/when expr in an arg list does not eat a sibling arg';
is @got.join(""), "A: two", 'given/when expr value is correct alongside a sibling arg';

@got = ();
@got.push("D: ", (given 3 { default { "d" } }));
is @got.join(""), "D: d", 'given/default expr in an arg list does not eat a sibling arg';

@got = ();
@got.push("P: ", (given 1 { when 99 { "a" }; when 1 { "b" } }));
is @got.join(""), "P: b", 'given with multiple when clauses picks the matching one only';

# The already-correct assignment form must not regress.
my $x = (given 2 { when 2 { "two" } });
is $x, "two", 'assignment-form given/when still yields exactly one value';

# An explicit `succeed EXPR` inside a `when` in expression position is a
# distinct code path from the implicit succeed-on-fallthrough above (both
# raise the same `is_succeed` signal, but this one carries an explicit
# return value rather than the body's last statement value).
is (given 2 { when 2 { succeed "explicit" } }), "explicit",
    'explicit succeed EXPR in expr-position given/when yields one value';
