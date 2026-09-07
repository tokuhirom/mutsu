# mutsu has no `PseudoStash` type — `CALLER::` reports `Stash`

Raku distinguishes a **package** symbol table (`Stash`) from a **pseudo-package**
view of a lexical pad (`PseudoStash`). mutsu collapses both into `Stash`.

## Repro

```
say CALLER::.^name;
```

- mutsu: `Stash`
- raku:  `PseudoStash`

## Why this is a prerequisite, not a cosmetic gap

`todo/deep/p5tie-stash-bind-key-protocol.md` asks for `Stash.BIND-KEY` /
`CALLER::.BIND-KEY` so that `P5tie` and `annotations` can bind a container into
a symbol table. That file treats the two cases as one piece of work. They are
not, and the reason is this ticket: because mutsu has no `PseudoStash`, the
package case

```
OUR.WHO.BIND-KEY(...)      # a real package Stash
```

and the lexical-pad case

```
CALLER::CALLER::.BIND-KEY(...)   # a pseudo-package view of a pad
```

are **the same mutsu value today**. Implementing `Stash.BIND-KEY` alone would
therefore either give the lexical case package semantics or give the package
case pad semantics. The two have to become distinct types first.

This is also the smaller, independently testable half: `CALLER::.^name`
answering `PseudoStash` is a one-line acceptance criterion that needs no
container-binding machinery at all.

## Scope — measured, 2026-09-07

mutsu gives **two different wrong answers**, not one, so this is not a single
mislabelled type. `say <SPELLING>.^name`:

| spelling | mutsu | raku |
|---|---|---|
| `CALLER::` | `Stash` | `PseudoStash` |
| `CORE::` | `Stash` | `PseudoStash` |
| `UNIT::` | `Stash` | `PseudoStash` |
| `MY::` | **`Hash`** | `PseudoStash` |
| `OUTER::` | **`Hash`** | `PseudoStash` |
| `DYNAMIC::` | **`Hash`** | `PseudoStash` |
| `LEXICAL::` | **`Hash`** | `PseudoStash` |
| `OUR::` | `Stash` | `Stash` ✅ |

So the lexical/dynamic views split into a group that reaches a `Stash` and a
group that is a bare `Hash` with no symbol-table type at all — the second group
is the further from correct. `OUR::` is the one spelling that is already right,
and it is right because it genuinely *is* a package stash.

**`PseudoStash` is not derived from `Stash`.** Measured:
`PseudoStash.^mro` → `((PseudoStash) (Map) (Cool) (Any) (Mu))`. So do not
implement it as a `Stash` subclass; both are `Map` descendants and they are
siblings. Check `SETTING::` and `CLIENT::` too — they were not measured here.

## Acceptance

- Every row of the table above matches raku, including `OUR::` staying `Stash`.
- `PseudoStash.^mro` matches raku's, i.e. `PseudoStash` is a `Map` descendant
  and **not** a `Stash` subclass.
- The existing `Stash` behaviour for package symbol tables does not change.
- A `t/` pin listing the pseudo-package spellings and the type each produces.
