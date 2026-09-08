# A `use` inside a block keeps a nested module's own imports

```raku
# Inner.rakumod:  unit module Inner; use NativeCall;
#                 sub inner-probe() is export { defined(&nativecast) ?? 'visible' !! 'MISSING' }
# Outer.rakumod:  unit module Outer; use NativeCall; use Inner;
#                 sub outer-probe() is export { inner-probe() }

use lib 'lib';
{ use Outer; }
use Outer;
say outer-probe();     # rakudo: visible    mutsu: MISSING
```

The block twin of the EVAL bug fixed in
`news/2026-09/module-loaded-in-an-eval-keeps-its-imports.md` — same symptom,
different mechanism. What went missing was `Inner`'s own view of `&nativecast`,
not the script's.

## Root cause

A `unit module`'s body runs at `current_package() == GLOBAL`, so its own
`use NativeCall` registers `GLOBAL::nativecast`. `pop_import_scope` retained from
`registry.functions` only keys that are package-qualified *and* not
`GLOBAL::`-prefixed, so that entry went with the enclosing block's import scope.
`loaded_modules` is never rolled back, so the later top-level `use Outer`
short-circuited and could not restore it: the module was left permanently
half-loaded.

## The fix

`pop_import_scope` now also retains a key that is in
`module_registered_functions`. That set already holds exactly the right thing:
its delta is taken **before** `import_module`, so an alias installed for the
*importing* scope is never in it, and `{ use Foo } foo()` still dies
(`roast/S11-modules/lexical.t`), as does `{ require NoModule <&bar>; }`'s `&bar`
(`roast/S11-modules/require.t` test 10). It is the block twin of the carve-out
`reinstate_module_functions` already gives the EVAL rollback, and it consults
the existing set rather than recomputing the rule.

Pinned by `t/block-use-keeps-nested-module-imports.t` (plus three fixtures under
`t/lib/`), which fails on the unfixed binary and passes unchanged under rakudo.

## Two side findings, filed separately

Writing that pin surfaced two things that are not this fix:

- **[#7611](https://github.com/tokuhirom/mutsu/issues/7611)** — a provider
  module named only inside a **comment** is loaded and imported. The first draft
  of the pin passed on an *unfixed* binary purely because its header comment
  said `use NativeCall`; the file now carries a `NOTE:` warning readers off that
  wording until the scan is driven off the AST instead of the source text.
- **[#7612](https://github.com/tokuhirom/mutsu/issues/7612)** — the opposite
  edge: a nested module's import is also visible to the *using* scope, where
  rakudo hides it. That is pre-existing (it reproduces with no block at all) and
  needs real lexical scoping for imports rather than another retain rule, so it
  is filed as `todo:deep`.

Closes #7580.
