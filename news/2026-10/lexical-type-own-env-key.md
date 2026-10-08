# Lexical types get their own env key

A `my class foo` (or `my role`) used to be bound in `env` under the bare name `foo`, the key a
same-named `$foo` shares, so `EVAL('foo.new')` next to `my $foo = 5` read the scalar. Lexical types
are now also bound under a type-only key (`lexical_type_key`), scoped like the bare binding, and the
bareword read consults it first. The `GetBareWordOverScalar` guard that fell back to the ordinary
resolution for names with a lexical type is gone (#12109).
