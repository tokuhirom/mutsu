# `nextsame` from a user `.new` now reaches the nearest built-in ancestor's constructor

```raku
class LoggedVersion is Version {
    method new(|c) {
        note "New version object created with arguments " ~ c.raku;
        nextsame;
    }
}
say LoggedVersion.new('1.0.2');
```

rakudo prints `v1.0.2`; mutsu printed `LoggedVersion.new` — the default gist of a bare, unpopulated
instance. The `note` line matched, so the override itself ran; only the onward dispatch was wrong.

## Root cause

`nextsame`/`callsame` advance through `resolve_deferral_expansion`, which walks the receiver's MRO
collecting **user-registered `MethodDef`s only**. mutsu implements built-in constructors natively
(`try_native_builtin_construct`), so `Version.new` is not a `MethodDef` and never appears in that
expansion. `class LoggedVersion is Version` therefore has exactly one `new` candidate, the user's,
and the chain is exhausted immediately.

`dispatch_next_candidate` (`src/runtime/builtins_dispatch_next.rs`) covers such holes with a ladder
of "native base candidate" shims — grammar `parse`, `Mu` `BUILDALL`/`clone`, the `Array`/`Hash`
storage protocols, `Any`'s `gist`/`Str`/`raku`, mixins, the metamodel. `new` was not one of them: it
had a separate special case that hardwired the fallback to `Mu.new`/`bless` on the invocant. That is
right for a plain user class but skips straight past a built-in ancestor that *does* have a
constructor, so the bless produced an attribute-less `LoggedVersion` instance instead of a `Version`.

## Fix

Added `native_builtin_new_next_candidate`: it walks the receiver's MRO, skips every user-declared
class, stops at `Any`/`Mu`, and hands the original arguments to the first ancestor for which
`try_native_builtin_construct` answers `Some`. It runs immediately before the `bless` fallback, so a
plain user class is untouched (`class Plain { method new(|c) { nextsame } }` still blesses). The
shim is self-limiting by construction: a built-in with no native constructor answers `None` and the
`bless` path still runs — no per-type list to maintain.

The constructor is selected by the *ancestor's* name, so an `Instance`-shaped builtin (`Date`,
`DateTime`, `Buf`, `IO::CatHandle`, ...) would come back tagged with the ancestor. `rebless_native_ctor_result`
re-tags it with the subclass the caller asked for, keeping the attributes the native constructor
computed — so `class MyDate is Date { method new(|c) { nextsame } }` yields a `MyDate`, as in rakudo.

`LoggedVersion.new('1.0.2')` now prints `v1.0.2` and `.parts` returns `(1 0 2)`; the same holds for
`callsame`, and for `class IntSub is Int { method new(|c) { nextsame } }`.

## Known remaining divergence

For built-ins backed by a native *scalar* `Value` variant (`Version`, `Int`, `Num`, `Str`, ...)
there is nowhere to put the class tag, so rakudo's `LoggedVersion.new('1.0.2').WHAT` of `(LV)` comes
back as `(Version)` in mutsu. That is the same limitation tracked by
`todo/tickets/str-subclass-loses-native-stringify.md` and is unchanged by this fix — the value and
every value-level method are now correct, only the type tag is not. `Instance`-shaped built-ins do
get the subclass tag (see above).

The sibling case of a built-in-derived class with *no* user `new` at all
(`class LV is Version { }; LV.new('1.0.2')`) still raises "Default constructor for 'LV' only takes
named arguments": that goes through `dispatch_new`'s `constructor_dispatch_name`, which uses the
receiver's own name and substitutes `__mutsu_user_class__` for any user-declared class. Widening
*that* to the nearest built-in ancestor is a larger change (it is the guard that keeps
`class Set is Hash {}` from constructing an immutable QuantHash) and was left alone.

Pinned by `t/multi-dispatch-ordering.t`.
