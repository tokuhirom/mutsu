# `MY::("x")` in term position, and registered routines missing from the `MY`/`LEXICAL` stash

The `&`-sigil indirect lookup through a pseudo-package now works
(`news/2026-08/caller-lexical-indirect-operator-lookup.md`). Two neighbouring
spellings of the same idea still do not. Neither blocks the real `Test` module,
which uses only `&CALLER::LEXICAL::(...)`, but both are ordinary Raku and both
show up in `roast/S02-names/pseudo-6e.t`.

## 1. A bare (sigilless) head has no symbolic-deref path

```
$ mutsu -e 'say MY::("x")'
===SORRY!=== Confused. expected statement: expected identifier after '::' ...
$ raku  -e 'say MY::("x")'
No such symbol 'MY::x'          # parses; resolves (and fails) at runtime
```

`GLOBAL::("Int")` behaves the same way. The `$`-sigil form is fine —
`parse_symbolic_deref_segments` (`src/parser/primary/var/scalar.rs`) is reached
from the scalar-var parser, so `$MY::("x")` works — and the `&` form now reaches
it too via `split_qualified_symbolic_head`
(`src/parser/primary/var/sigil_vars.rs`). Only the bare head lacks it.

The postfix `expr::(key)` -> `expr.WHO{key}` desugar
(`src/parser/expr/postfix/loop_.rs`) is what *should* pick this up, and it does
fire for a single segment in some positions, but the identifier parser consumes
`MY::` first and then finds no identifier after the `::`. Note the semantics of
the two routes differ: the postfix desugar produces a stash index, whereas the
`$`/`&` route produces a `SymbolicDeref` that selects a *scope*. Decide which one
a bare head should get before implementing (raku's error message, `No such
symbol 'MY::x'`, is the `SymbolicDeref` shape).

## 2. The `MY`/`LEXICAL` pseudo-stash does not contain registered routines

```
$ mutsu -e 'sub f($a){$a*2}; say MY::{"&f"}(3)'
Impossible coercion from 'Int' into 'Any': no acceptable coercion method found
$ raku  -e 'sub f($a){$a*2}; say MY::{"&f"}(3)'
6
```

The stash is built from `code.locals` plus `env`
(`src/vm/vm_var_assign_local.rs`, around the `MY`/`LEXICAL` case), and a `sub f`
lives in the routine registry rather than in `env`, so `MY::{'&f'}` answers
`(Any)` where raku answers `&f`. Registered routines visible at that point have
to be folded in. `&MY::("f")` is unaffected because it goes through
`resolve_code_var`, which does consult the registry.
