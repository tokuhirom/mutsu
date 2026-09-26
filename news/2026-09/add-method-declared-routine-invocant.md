# `^add_method` binds the invocant for declared routines and `$self` blocks

Code handed to `^add_method` that is not a method receives the invocant as its
first positional parameter. Two shapes got that wrong (issue #9549):

- A declared routine (`E.^add_method('m', &named-sub)`, a `my sub`) died
  with "Too few positionals passed", because the invocant was dropped.
- A pointy block whose first parameter is literally `$self`
  (`-> $self, $a {...}`) read `$self` as Nil.

`add_method` used to give a block its invocant by prepending `my $x := self` to
the block's AST body. A declared routine has no AST body (its bytecode lives in
`compiled_routine`), and the alias for `self` itself bound nothing usable.
Now the first positional parameter is instead marked as the method's invocant
parameter, and the method binder binds it to the receiver by name. This is the
same mechanism that already serves `method ($inv: ...)`. The code itself is no
longer rewritten, so the AST alias is gone for blocks too. Code with a
names-only signature (a one-parameter `-> $r {...}`, a builtin such as
`&[cmp]`) gets a `ParamDef` for that first parameter so it can be marked.
