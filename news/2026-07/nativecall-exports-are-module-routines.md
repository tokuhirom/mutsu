# NativeCall's exports are module routines, not global builtins

`nativecast` and `nativesizeof` were dispatched in mutsu as **unconditional
builtin functions** (`Interpreter::try_nativecast` / `try_nativesizeof`, reached
from `vm_call_func_ops`'s function-dispatch chain). They are not builtins in
Raku: Rakudo exports them from `NativeCall.rakumod`, alongside `cglobal`,
`explicitly-manage` and `refresh`.

```raku
# rakudo/lib/NativeCall.rakumod
our sub nativesizeof($obj) is export(:DEFAULT) { … }
our proto sub nativecast(|) is export(:DEFAULT) {*}
our sub cglobal($libname, $symbol, $target-type) is export is rw { … }
```

This is exactly the distinction CLAUDE.md's working agreement draws: a function
belongs in the builtin set only if `raku-doc/doc/Language/perl-func.rakudoc`
lists it. Neither does; they are documented under
`Language/nativecall.rakudoc`'s "Helper functions" instead. Two consequences
were observable:

```raku
# 1. Visible without importing the module.
say nativesizeof(int64);                   # was: 8         raku: Undeclared routine
# 2. Not first-class -- no `&nativecast` to take, pass or wrap.
say defined(&nativecast);                  # was: MISSING   raku: True
```

The second is the one that bites real code: a distribution that passes
`&nativecast` around, or that looks it up as `::('&nativecast')`, found nothing.

## What changed

All five helpers are now ordinary `our sub`s in the NativeCall prelude, and the
marshalling stays in Rust behind a `__mutsu_`-prefixed primitive that is not part
of the user-visible surface — the pattern `cglobal` established. The five prelude
sources live in one table, `NATIVECALL_SUB_PRELUDES`, and
`inject_nativecall_subs_prelude` injects each entry independently, so a program
that declares its own `sub refresh` (a common enough name) still receives the
other four.

Every entry is written `is export`, as Rakudo declares them. That is
load-bearing rather than decoration, and finding out why was the interesting
part of this change: a prelude is spliced into the *host* compunit, so inside a
`unit module M` a plain `our sub` registers as `M::nativesizeof` and is invisible
to a method body running under some other package. `NativeHelpers::Pointer` is
exactly that shape — it `^add_method`s pointer arithmetic onto
`NativeCall::Types::Pointer` and calls `nativesizeof` from inside the added
method — so it broke the moment `nativesizeof` stopped being ambient. `is export`
also registers the routine globally, which is what the `GLOBAL::` prefix already
does for the prelude's classes.

One deviation survives, and it is the price of that workaround: because `is
export` registers globally, a program that loads *some other* module which uses
NativeCall can still see `nativesizeof` without importing NativeCall itself.
Rakudo would not. The case that actually mattered — a program that imports
nothing and calls it — now fails, as it should.

The underlying gap that made this necessary is recorded separately as
[`todo/deep/module-package-sub-invisible-from-method-body.md`](../../todo/deep/module-package-sub-invisible-from-method-body.md):
mutsu resolves an unqualified routine name by *package*, so a method body cannot
see its own compunit's package-scoped subs the way Raku's lexical scoping
requires. That affects every module in the batteries, not just NativeCall's
prelude.

## The injection guard had to stop reading the source text

The first cut gated each entry on `!source.contains("sub <name>")`. That also
matches the phrase in a *comment* — and `t/nativecall-explicitly-manage.t`
documents its subject as "as Rakudo's `sub refresh($obj --> 1)` does", so the
file testing `refresh` silently lost it. The guard now asks the parsed statements
for a top-level `Stmt::SubDecl` of that name instead, which is the only scope an
injected top-level `our sub` can actually clash with.

## Pin

`t/nativecall-exports-are-routines.t` checks all five are real routines
(`::('&name')`), that `&nativecast` / `&nativesizeof` can be bound, called
through the `&` form and used as a `map` body, that the awkward argument shapes
survive parameter binding rather than reaching a builtin raw (a type object, a
parameterized `CArray[uint8]`, and a `:(num64 --> num64)` Signature literal), and
that a program which does not `use NativeCall` fails instead of answering.

Related: [`todo/tickets/nativecall-surface-gaps.md`](../../todo/tickets/nativecall-surface-gaps.md),
whose remaining item is the cosmetic `NativeCall::Types::` prefix on the type
objects' `.^name`.
