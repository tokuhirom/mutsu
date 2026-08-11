# Battery: slang activation — `Slangify` + `Slang::Tuxic`

**Slot:** Slang activation (ADR-0026) · **Chosen:** `Slangify` (upstream
`zef:lizmat`, v0.0.4, Artistic-2.0) + `Slang::Tuxic` (upstream
`zef:raku-community-modules` / Tux, v0.0.5, Artistic-2.0) · **Kind:** Bundled,
run verbatim (`modules/Slangify/`, `modules/Slang-Tuxic/`)

## What they are

`Slang::Tuxic` is a genuine Raku slang: it mixes two roles into the live
grammar to allow whitespace between a call target and its opening parenthesis
— H.Merijn Brand (Tux)'s personal style, which `Text::CSV` uses on every one
of its ~500 call sites:

```raku
use Slang::Tuxic;

sub foo ($a, $b) { $a * $b }
say foo (3, 5);        # 15 — a two-argument call, NOT a one-List listop
say 42.fmt ('-%d-');   # -42- — spaced .method (args) is a method call
```

`Slangify` is the registration surface: a slang module writes
`use Slangify TheRole, Mu, TheLegacyRole, Mu;` and Slangify's EXPORT
generator exports an inner `&EXPORT` into it. That inner EXPORT runs at the
*user's* compile time and installs the mixin:
`$*LANG.define_slang('MAIN', $*LANG.slang_grammar('MAIN').^mixin($role), ...)`.

## How the verbatim modules run under mutsu (ADR-0026)

mutsu's parser is a hand-written Rust recursive descent, so "a role was mixed
into the MAIN grammar" is *interpreted* rather than executed: the roles'
overridden **rule names** map onto hand-implemented parser modes. The chain:

1. The parser meets `use X` where X's source directly `use`s Slangify (the
   activation gate, checked on the cached module scan).
2. X's whole load runs at parse time in a fresh interpreter on a fresh
   thread (`runtime/slang_activation.rs`) with a compile-time `$*LANG`
   object (`Mutsu::Slang::CompLang`) bound. Slangify's inner `&EXPORT` runs
   verbatim — mutsu executes its `.^name` check (a non-`Raku::` name, so the
   legacy/NQP role set is selected, per the ADR), the `.^mixin` recording,
   and `define_slang`.
3. `define_slang` reads the recorded roles' declared `token`/`rule` names
   and maps each onto a parser mode via the recognized-override map
   (`parser/stmt/simple/slang_modes.rs`): `term:sym<identifier>` →
   spaced-call, `methodop` → spaced-methodop,
   `routine-declarator:sym<sub>`/`routine_declarator:sym<sub>` → no-op. An
   unrecognized rule is a **hard compile-time error** naming the rule.
4. The rest of the using compilation unit parses in the changed mode. Slang
   state is lexical to the unit: importers of a module that uses the slang
   are unaffected, and EVAL strings parse in the stock grammar.

Pinned by `t/slang-tuxic-activation.t` (rakudo-verified, including the
scoping assertions) and the upstream `Slang-Tuxic` suite in the release
batteries gate (8/8).

## Status / limitations

- `Slang::Tuxic` upstream suite: **8/8 green** (whitelisted in the release
  gate).
- `Slangify`'s own upstream test uses a fixture slang (`t/Piersing.rakumod`)
  that overrides the `identifier` and `name` rules (trailing `?`/`!` on
  identifiers). Those rules are not in the recognized-override map yet, so
  the activation fails loudly with the NYI error — the file is not
  whitelisted. Adding the two rules (a third parser mode) is the natural
  next slice if Piersing-style slangs matter.
- Any future Slangify-based slang gets the registration surface for free;
  only its override rules need adding to the map.
- The `Text::CSV` campaign (docs/batteries/csv.md) is the consumer this
  battery exists for: `use Text::CSV` now clears the `use Slang::Tuxic;`
  parse barrier.
