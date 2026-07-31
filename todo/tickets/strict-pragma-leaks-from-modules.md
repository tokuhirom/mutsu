# `use strict` inside a module leaks to the importing program

A module whose source says `use strict` turns the interpreter-wide
`strict_mode` flag on for the *importing* program too:

```
# lib/StrictMod.rakumod:  use strict; unit module StrictMod; ...
mutsu -I lib -e 'use StrictMod; $leak = 5; say $leak'
# X::Undeclared: Variable '$leak' is not declared
```

`runtime_module.rs` saves/restores `strict_mode` around a module load, so
either the restore does not cover this path or the pragma is applied after
the restore point. In raku, `use strict` is lexical.

Two mitigating facts, which is why this is a ticket and not a bug being fixed
on the spot:

- **raku is strict by default** — a bare `$leak = 5` is a compile error in
  raku with or without any module. So the leak usually moves mutsu *closer*
  to raku behavior, and the day mutsu flips its default to strict the leak
  becomes unobservable for `use strict` (only a `no strict` module leaking
  would matter).
- The expression-position-declaration false positive that made the leak
  visible in Humming-Bird's suite was a separate genuine bug, fixed with
  `t/strict-expr-position-decl.t` (MarkVarDeclContext exemption).

Found while bisecting Humming-Bird t/03 (2026-07-31 web-framework survey
follow-up): a bisect variant *without* its own `use strict` still ran strict
after `use Humming-Bird::Core` (whose source uses strict).
