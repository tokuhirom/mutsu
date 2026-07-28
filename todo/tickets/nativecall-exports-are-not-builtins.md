# NativeCall's exports are global builtins in mutsu, and not `&`-callable

`nativecast` and `nativesizeof` are dispatched in mutsu as **unconditional
builtin functions** (`Interpreter::try_nativecast` / `try_nativesizeof`, reached
from `vm_call_func_ops`'s function-dispatch chain). They are not builtins in
Raku: Rakudo exports them from `NativeCall.rakumod`.

```raku
# rakudo/lib/NativeCall.rakumod
our sub nativesizeof($obj) is export(:DEFAULT) { … }
our proto sub nativecast(|) is export(:DEFAULT) {*}
our sub cglobal($libname, $symbol, $target-type) is export is rw { … }
```

This is exactly the distinction CLAUDE.md's working agreement draws: a function
belongs in the builtin set only if `raku-doc/doc/Language/perl-func.rakudoc`
lists it. Neither does; they are documented under
`Language/nativecall.rakudoc`'s "Helper functions" instead.

Two observable consequences:

```raku
# 1. Visible without importing the module.
say nativesizeof(int64);          # mutsu: 8      raku: Undeclared routine
# 2. Not first-class -- there is no `&nativecast` to take, pass or wrap.
say defined(&nativecast);         # mutsu: MISSING    raku: True
use NativeCall; say &nativesizeof.arity;   # mutsu: no such routine
```

The second is the one that can bite real code: a distribution that passes
`&nativecast` around, or that checks `::('&nativecast')`, finds nothing.

## The shape of the fix

`cglobal` (added 2026-07-29) is the pattern to follow, and it exists precisely
because of this rule: the user-visible sub is a **Raku definition in the
NativeCall prelude** (`run::NATIVECALL_CGLOBAL_PRELUDE`, injected by
`inject_cglobal_prelude` only when the source `use`s NativeCall and names it),
and the native half is a mutsu-private primitive (`__mutsu_cglobal_fetch`).

Moving `nativecast` and `nativesizeof` the same way makes them importable rather
than ambient, gives them a real `&`, and leaves the marshalling in Rust:

```raku
our proto sub nativecast(|) {*}
multi sub nativecast($target-type, $source) { __mutsu_nativecast($target-type, $source) }
our sub nativesizeof($obj) { __mutsu_nativesizeof($obj) }
```

Do this only with the compatibility risk in mind: code that currently calls them
without `use NativeCall` would start failing, which is *correct* but is a
behaviour change. Grep the batteries and `t/` first, and land it with the
prelude-injection gate widened to cover the new names.

Related: [`nativecall-surface-gaps.md`](nativecall-surface-gaps.md) (what else is
missing from the surface).
