# A user class named `void`/`long`/`size_t` reports a `NativeCall::Types::` name

Declaring a class whose name collides with a `NativeCall::Types` entry makes
`.^name` report the NativeCall-qualified name, even in a script that never
mentions `NativeCall`.

## Repro

```
class void { }
say void.^name;
```

- mutsu: `NativeCall::Types::void`
- raku:  `void`

No `use NativeCall` is required for this to happen.

## Scope — measured, 2026-09-07

`class <NAME> { }; say <NAME>.^name`:

| name | mutsu | raku |
|---|---|---|
| `void` | `NativeCall::Types::void` | `void` |
| `long` | `NativeCall::Types::long` | `long` |
| `ulong` | `NativeCall::Types::ulong` | `ulong` |
| `size_t` | `NativeCall::Types::size_t` | `size_t` |
| `int8` | `int8` ✅ | `int8` |

`int8` being correct is the useful discriminator: it is a *native type* that
already exists in the core language, so it takes a different path. The failing
names are the ones supplied only by the NativeCall type table.

Only the type declaration is affected. A `sub void() { 42 }` and a
`my $void = 1` both behave normally, so this is class-declaration name
resolution, not a general reservation of the identifier.

## Why it matters

`.^name` is load-bearing in every `X::` message and in `.raku`/`.gist` output,
so a user type that happens to be called `long` renders under a package it was
never declared in. It is narrow — these are unusual class names — but it is a
*wrong answer* about user-declared code, produced by machinery for a module the
program does not use.

It is also the one actionable residue that
`todo/deep/nativecall-cannot-be-vendored.md` recorded and explicitly declined
to file separately; that file is a decision record and should not carry a live
bug.

## Where to look

`src/runtime/nativecall*.rs` (six files, ~2566 lines) registers the
`NativeCall::Types::*` entries. The declaration path for a `class` must prefer
a user declaration over that table — most likely the table is consulted by bare
name during type registration or `.^name` rendering, without checking whether
the current compilation unit declared the name itself. Compare against the
`int8` path, which resolves correctly.

## Acceptance

- Every row of the table above matches raku.
- `use NativeCall; my long $x` and the other real NativeCall usages keep
  working — the vendored/native provider's own types must still resolve when
  the user has *not* declared a colliding name.
- A `t/` pin declaring a class for each colliding name and asserting `.^name`.
