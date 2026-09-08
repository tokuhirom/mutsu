# mutsu has a `PseudoStash` type

Raku keeps a **package** symbol table (`Stash`) apart from a **pseudo-package**
view of a lexical pad (`PseudoStash`). mutsu had only the first, and the
spellings that never reached a stash at all answered a bare `Hash`:

| spelling | before | raku |
|---|---|---|
| `CALLER::` `CALLERS::` `CORE::` `UNIT::` `SETTING::` `CLIENT::` `OUTERS::` | `Stash` | `PseudoStash` |
| `MY::` `OUTER::` `DYNAMIC::` `LEXICAL::` | `Hash` | `PseudoStash` |
| `OUR::` `GLOBAL::` `PROCESS::` | `Stash` | `Stash` |

All fourteen rows match rakudo now, including the three that are genuine
package symbol tables and stay `Stash`.

## Why the type had to exist first

[#7588](https://github.com/tokuhirom/mutsu/issues/7588) filed this as a
prerequisite, not a cosmetic gap. `Stash.BIND-KEY` / `CALLER::.BIND-KEY` — what
`P5tie` and `annotations` need to bind a container into a symbol table — has to
bind into a *package* in one case and into a *lexical pad* in the other. While
`OUR::` and `CALLER::CALLER::` were the same mutsu value, implementing one
would have given the other the wrong semantics. They are distinct types now, so
that work can proceed.

## What it is, and what it is not

Measured against rakudo: `PseudoStash.^mro` is
`(PseudoStash Map Cool Any Mu)`. It is a **sibling** of `Stash` under `Map`, not
a subclass, so it must never be modelled as `class PseudoStash is Stash`. The
row added to `builtins/builtin_type_catalog.rs` (the ADR-0051 single source of
truth for built-in ancestry) says exactly that, and `t/pseudostash-type.t`
asserts `Stash` does not appear anywhere in the MRO.

## How it is represented

The two spelling groups reach different internal representations, and each
keeps the one it had:

- `CALLER::` and the other package-stash-backed spellings are instances
  carrying a `symbols` attribute. `Interpreter::stash_class_for_package` now
  picks the class from the package name, so `make_stash_instance` mints a
  `PseudoStash` for a pseudo-package spelling and a `Stash` for a real one —
  one decision point, every caller included. Every code path that used to ask
  `class_name == "Stash"` asks `crate::value::types::is_stash_class_name`
  instead, so a pseudo-stash keeps all the behaviour a stash had (`.keys`,
  `:exists`, `BIND-KEY`, `EVAL ..., context => $stash`, identity under
  `unique`/`eqv`).
- `MY::`, `OUTER::`, `LEXICAL::` and `DYNAMIC::` are pad snapshots kept as
  ordinary hashes — their `.keys`, `.{...}` and iteration all ride the Hash
  paths — so the type travels as the hash's declared type, exactly the way
  `Map` already does.

Unifying those two representations is a bigger job than unifying the reported
type, and nothing here needs it; the type predicate answers the same for both.

`Stash`'s own MRO stays `(Stash Any Mu)` where rakudo has
`(Stash Hash Map Cool Any Mu)`. That is a separate, pre-existing divergence and
the ticket explicitly asked for existing `Stash` behaviour to be left alone.

## Found on the way

Writing a pin that passes under rakudo as well as mutsu turned up a second
divergence: `@a>>.^name` distributes the metamethod in mutsu but applies
`.^name` to the container in rakudo (`"Array"`). Filed as
[#7643](https://github.com/tokuhirom/mutsu/issues/7643); the pin uses
`.map(*.^name)` so it is portable.

Closes [#7588](https://github.com/tokuhirom/mutsu/issues/7588).
