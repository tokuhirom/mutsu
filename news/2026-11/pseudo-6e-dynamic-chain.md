# 6.e `DYNAMIC::` sees only dynamic symbols and ends in GLOBAL, then PROCESS

Under `use v6.e.PREVIEW`, the dynamic chain behaves as in rakudo 2026.11:

- `DYNAMIC::<$x>` of a caller's plain lexical is an `X::Symbol::NotDynamic` Failure. Only a
  `$*` twigil, an `is dynamic` variable, `$/` or `$!` is visible. `$_` is no longer
  dynamic in 6.e.
- `CALLERS::<$x>` of a non-dynamic is the same Failure instead of a thrown
  `X::Caller::NotDynamic`. `CALLER::` still throws.
- `DYNAMIC::.keys` no longer lists `$_`.
- A `$*x` that no frame declares falls back to `GLOBAL::<$x>` before `PROCESS::<$x>`, both
  when read (`DYNAMIC::<$*x>`) and when listed (`DYNAMIC::.keys`, `:exists`).
- `DYNAMIC::<$*x> := v` binds the GLOBAL or PROCESS variable the chain finds.

6.c and 6.d keep their behavior. `S02-names/pseudo-6e.t`'s "Non-dynamic failures" and
"Dynamic is only dynamic" subtests now pass (#18742).
