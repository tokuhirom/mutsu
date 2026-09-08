# A `use` inside a block drops a nested module's own imports

The block twin of the EVAL bug fixed in
`news/2026-09/module-loaded-in-an-eval-keeps-its-imports.md`. Same symptom, same
principle, a different mechanism — and unlike the EVAL half, nothing in roast or
the bundled libraries depends on it today, so it is a ticket rather than a
blocker.

```raku
# Inner.rakumod:  unit module Inner; use NativeCall;
#                 sub inner-probe() is export { defined(&nativecast) ?? 'visible' !! 'MISSING' }
# Outer.rakumod:  unit module Outer; use NativeCall; use Inner;
#                 sub outer-probe() is export { inner-probe() }

use lib 'tmp/nclib';
{ use Outer; }
use Outer;
say outer-probe();     # rakudo: visible    mutsu: MISSING
```

`Inner`'s own view of `&nativecast` is what goes missing — the nested module's
import, not the script's.

## Mechanism

`pop_import_scope` (`src/runtime/runtime_module.rs`) retains from
`registry.functions` only

```rust
ks.contains("::") && !ks.starts_with("GLOBAL::")
```

and a `unit module`'s body runs at `current_package() == GLOBAL`, so `Inner`'s
own `use NativeCall` registered `GLOBAL::nativecast` and is dropped when the
enclosing block's import scope pops. `loaded_modules` is never rolled back, so
the later `use Outer` short-circuits and cannot restore it.

Verified by disabling that retain: the block row becomes `visible`, and the EVAL
row (a separate path) does not move.

## The shape of the fix

Exactly the carve-out the EVAL half now has. `module_registered_functions`
already records every routine a module load registered, including — since that
fix — the `GLOBAL::` aliases installed while the module's own body ran.
`pop_import_scope`'s retain needs to consult that set, so a key belonging to a
loaded module survives the pop while a key the *popping scope* imported does not.

The constraint to respect is `roast/S11-modules/lexical.t`: `{ use Foo }` must
still leave `foo()` unresolvable outside the block. The EVAL half keeps that
distinction by timing (the delta is taken before `import_module`, so the
importing scope's own alias is never in the set), and the same set therefore
already has the right contents for this — it should be a matter of consulting it
rather than recomputing the rule.

## Reproducing

Write the two modules under `tmp/nclib/`, then the script above, and compare
against `raku -Itmp/nclib`.
