# `.^add_method` installs a routine-backed code object, instead of an empty method

```raku
class C { }
C.^add_method('m', my method m5() { 5 });
C.^compose;
say C.m();      # raku: 5   mutsu was: Nil
```

`.^add_method` silently installed a method with an **empty body** whenever the
code object it was handed was a *named, separately-declared routine*. The method
was genuinely registered — `.^can` found it, `.^lookup` returned a defined
`Method`, `.^methods` counted it, and its `.name` and `.signature` read back
plausibly — and the call just answered `Nil`, with no error anywhere.

Every *anonymous* shape worked: `method () {...}`, `my method () {...}`,
`anon method n() {...}`, a pointy block. That split is what made the bug hard to
see from the outside, and it is also why the batteries never hit it.

## What the ticket said, and what was actually true

`todo/deep/direct-metamodel-classhow-new-type-immutable-error.md` had been
narrowed to "a method-dispatch gap on a `new_type`-minted `Package`". It has
nothing to do with `new_type`: an ordinary `class C { }` shows it identically.
What decides the outcome is the shape of the code object, not the shape of the
type it is being added to.

The code object was never at fault either — calling the very same
`my $m = my method m() { 42 }` through `$m()` returned `42`.

## Root cause

`add_method` builds its `MethodDef` from the Sub's AST `body` plus its
`compiled_code`. A Sub built from a **declared routine** — `&foo`, a
`.candidates` entry, a `my sub` / `my method` read back through `GetCodeVar` —
carries its bytecode in `SubData::compiled_routine` instead. The two fields are
kept deliberately apart, because they are invoked under different calling
conventions, and ADR-0019 C6c stopped the sub declaration plan shipping an
executable AST at all. `MethodDef` has no `compiled_routine` field, so the def
came out with an empty body and no code.

It is visible straight from `--dump-bytecode`: a named `my method a() { 42 }` in
expression position compiles to `RegisterDecl(0); GetCodeVar("a")` plus a
separate `sub GLOBAL::a` chunk, while the anonymous form compiles to a single
`MakeAnonSubParams` that carries the body with it.

The fix takes the routine's own `CompiledCode` when the Sub is routine-backed and
carries no closure body. The method ABI drives parameter binding from the
`MethodDef`'s own `params`/`param_defs`, which `add_method` already fills in from
the Sub, so the routine's bytecode runs correctly under it — verified across the
shapes that actually exercise the convention: reading `self`'s attributes,
positional parameters, a named parameter and its default, a `:D` invocant
constraint, and an explicit `return` in the body.

`t/add-method-named-routine.t` pins 16 rows against `raku` v2026.07, including
the four anonymous shapes that already worked and the registration
introspection that was always right.

## Two things measured alongside, deliberately left open

Both are recorded in the ticket, which stays open for them:

- **The installed method reports the name it was added under**, where raku
  reports the routine's own name: `A.^lookup('m').name` is `m` in mutsu and `m9`
  in raku. Kept as a `todo` row in the pin.
- **A plain `sub` used as a method does not receive the invocant as its first
  positional.** `B.^add_method('p', &plain)` with `sub plain($x)` answers `42`
  for `B.p(21)`; raku rejects the call, because the invocant occupies the first
  slot. It answered `Nil` before this change, so it was wrong either way — but
  now it is wrong *loudly enough to be worth fixing separately*.
