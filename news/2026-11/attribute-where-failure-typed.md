# A failing attribute `where` throws X::TypeCheck::Assignment

`has $.bar where * > 2` failing at construction raised an untyped `X::AdHoc` ("... where constraint
failed"). It now throws `X::TypeCheck::Assignment` with rakudo's message (`expected <anon> but got
Int (1)`), so `try ...; $!  ~~ X::TypeCheck` works as in rakudo. WhereList's `t/01-basic.t` test 7
depends on it.
