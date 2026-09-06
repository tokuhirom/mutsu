# A native provider's exports are symbolically navigable

```raku
use Test;
my &mmk = ::("Test::EXPORT::DEFAULT::&ok");
say &mmk;
```

Raku models a used module's exported symbols as a real nested package,
`Mod::EXPORT::DEFAULT`, reachable like any other package. mutsu answered
`No such symbol 'Test::EXPORT::DEFAULT::&ok'`.

## The ticket generalised from one module

`todo/deep/export-default-package-not-symbolically-navigable.md` concluded that
mutsu "copies exported symbols directly into the importing scope … but never
materializes the `ModuleName::EXPORT::DEFAULT` package itself as a queryable
stash". Measured against raku v2026.07, that is true of **`Test` only**. For a
module loaded from source the entire path already matched raku byte for byte —
the `EXPORT` package listing its tags, the tag stash holding the routines, the
qualified symbolic lookup resolving, the routine callable through it, and the
module's own `.WHO` listing `EXPORT` — including for a module whose own name
contains `::`.

`Test` is different because it is a **native provider**. It runs no `is export`
declarations, so nothing populated `exported_subs` for it, and
`package_stash_value` builds the `EXPORT` stashes from exactly that table.
`NativeCall` never had the gap for the same reason in reverse: it already had a
`register_nativecall_exports` doing this by hand.

## The fix

`register_native_provider_exports`, the general form of the NativeCall helper,
called for `Test` at load time.

That alone built the stash with the right *keys* and `Nil` behind each one. The
second half: `package_stash_value` resolves each export as `Mod::name`, and a
native provider's routines are registered under their **bare** name — there is no
`Test::ok` `FunctionDef` to find. It now falls back to the bare name when the
qualified lookup comes back `Nil`, so the stash entry carries the routine rather
than a hole.

`t/export-default-stash.t` pins nine rows and passes verbatim under rakudo too:
the native-provider path, that the resolved value is the real routine (`.name` is
`ok`), and the source-module path that was already correct — pinned because the
ticket claimed it was broken, and it is the half most likely to break if the
stash construction is touched again.

## What is left

Two rows measured alongside and deliberately not fixed:

- **`JSON::Fast` / `JSON::Tiny` cannot use this.** Their `to-json` / `from-json`
  are native builtins with no code-var form at all, so registering their export
  names would build a stash whose every entry resolves to `Nil` — worse than
  having no stash. Giving those two a code-var form is the prerequisite.
- **`Test.WHO.keys` still differs.** raku lists `EXPORT` plus a handful of
  `&`-subs; mutsu lists the `Test::` submodule classes. That is the module
  stash's own composition rather than the export machinery, and a user module's
  `.WHO` already answers correctly (pinned).
