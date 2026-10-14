# `my $x .= meth` calls the method on the new variable

An untyped `my $x .= meth(...)` is `my $x = $x.meth(...)`, where `$x` is the
variable being declared: an `Any`, an empty Array or Hash, or its
`is default(...)` value. mutsu evaluated the call as the declaration's
initializer, before the new lexical was bound. A same-named variable of an
outer scope therefore became the invocant: `my $q = "x"; { my $q .= Str }` gave
`"x"` instead of `""`, and `{ my @a .= push(3) }` pushed onto the outer `@a`.

The declaration now lowers to "declare, then assign the call on the declared
variable" (`ast::method_assign_decl::expanded_declaration`), on both the
parser and the RakuAST paths. `state`, `our`, dynamic, typed and shaped
declarations keep the single-initializer form.

Closes #13273.
