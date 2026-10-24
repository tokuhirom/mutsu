# A hand-built assignment lowers through the one assignment lowering

`EVAL` of a hand-built `ApplyInfix(Assignment)` used three readers of its
own in `rakuast::lower`: `subscript_assign`, `method_call_assign` and
`call_assign`. Each re-implemented part of the parser's per-target dispatch.
They are gone (#14948). A subscript, method-call, call or parenthesised
target now lowers through `rakuast::assign_lower::lower_expression`, the
same function the parser's typed assignment node lowers through.

Only two cases keep a reader of their own:
- a symbolic dereference (`$::($n) = v`);
- an immutable literal (`120 = 3`), which throws after the right side runs.

The dead helpers this exposed, `paren_list_assign_expr` and a second copy of
`list_lvalue_assign_expr`, were deleted too.
