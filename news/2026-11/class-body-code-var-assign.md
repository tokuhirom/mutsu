# A class body's `my &t` assigned later is callable

`class S { my &t; &t = sub { 1 }; say t() }` died with
`Unknown function: t`. The class-body statement `&t = ...` was compiled as an
assignment to the package variable `&S::t`, because the package-qualification
step did not count a class body's `my &code` variables as lexicals the way it
counts its other `my` variables. The assignment now fills the lexical, so
`t()`, `&t()` and the class's methods see it, as in rakudo (#19143). This is
the shape Timezones::ZoneInfo's `State.rakumod` uses.
