# A user variable trait named like a parser marker is applied

`my $x is __x` ignored the trait: the parser files its own markers
(`__has_initializer`, `__scalar_bind`, ...) in `VarDecl::custom_traits` under a `__`
prefix and every consumer filters on it, so a user trait starting with `__` was dropped
as one. A user trait with that spelling is now recorded under an escape prefix
(`ast::escape_user_trait`), the same idiom as the sigilless `term_names` prefix, and
unescaped where `ApplyVarTrait` looks the handler up (#14532).

The other half of the ticket, a variable trait directly in a role body running once per
composition instead of once, is left open: a role body's variables are not walked by the
BEGIN-prologue lift, so they have no static cell to apply the trait over.
