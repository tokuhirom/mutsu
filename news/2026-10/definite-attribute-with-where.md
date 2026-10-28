# `has Str:D $.x where ...` needs no initializer

rakudo treats `has Str:D $.stage where * ~~ any <before around after>` as a constrained subtype and
does not demand an initializer for it (only the bare `has Str:D $.x` is `X::Syntax::Variable::MissingInitializer`).
Found loading OO::Plugin; its tests need `Test::Async`, which only the legacy frontend runs, so the
distribution is `no_baseline`.
