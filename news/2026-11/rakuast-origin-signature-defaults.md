# RakuAST origins of parameters with defaults and markers

The signature reader that places the parameters of a pointy block, a regex
declaration and a `my (...)` declaration now reads a parameter with a default
(`$x = 42`, `:$c = 42`, `token w(:$e = 42)`) and one written with a `?` or `!`
marker (`$x?`, `:$y!`): the parameter runs through its marker and its
default, as in rakudo 2026.11. Such a signature used to get no ranges at all
(#15780).
