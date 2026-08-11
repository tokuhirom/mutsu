# Slang activation ships: bundled Slangify + Slang::Tuxic run verbatim (ADR-0026)

`use Slang::Tuxic;` now works: the upstream slang modules are bundled as
batteries and executed for real, and the rest of the using compilation unit
parses with Tuxic's grammar changes (`foo (3, 5)` is a two-argument call,
`.method (args)` is a method call). The upstream `Slang-Tuxic` test suite
passes 8/8 and is gated. This clears the parse barrier that kept `Text::CSV`
— the deepest-tested CSV candidate, which `use`s the slang unconditionally —
from even loading (`todo/deep/text-csv-needs-slang-tuxic-support.md`).

The campaign landed as four PRs across two days (ADR-0026, Accepted
2026-08-11):

- **#6273 — parser slang modes (§2.3).** Unit-scoped `SlangModes` parser
  state (reset at parse start, snapshot/restored around nested module scans)
  gating the two Tuxic behaviors: spaced identifier calls (with the token's
  own keyword/type-name exclusion list) and spaced methodops, in both the
  postfix chain and regex-embedded method calls. Plus the recognized-override
  map: rule name → mode, unknown rule → hard error.
- **#6274 — regex scanner fix (prerequisite).** A character class nested in a
  lookaround (`<!before '"' <-["]>*? >` — the exact shape in Tuxic's
  `methodop` token) desynced quote/angle tracking in both the regex-literal
  scanner and the lookaround-body parser; the whole module failed to parse.
  General fix, rakudo-verified, pinned by `t/regex-charclass-in-lookaround.t`.
- **#6275 — Slangify-style inner `&EXPORT` (prerequisite, §1.2).** An
  `&EXPORT` a module imports from another module's EXPORT map becomes that
  module's own EXPORT for its importers, and `sub EXPORT` reruns on every
  import (not once per process). Slangify's entire registration mechanism is
  this pattern; measured against rakudo with an instrumented copy.
- **The activation slice (§2.1 + §2.2 + §2.4).** When the parser meets
  `use X` where X's source directly `use`s Slangify, it executes X's whole
  load in a fresh interpreter **on a fresh thread** (clean parser
  thread-locals — no save/restore of the outer in-progress parse), with a
  compile-time `$*LANG` (`Mutsu::Slang::CompLang`) bound. Slangify's inner
  EXPORT runs verbatim: the `.^name` legacy-selection check, `.^mixin`
  recording on opaque grammar handles, and `define_slang`, which maps the
  roles' declared token names onto the §2.3 parser modes and errors on
  anything unrecognized. The rule names return via the thread join; the hook
  sets the modes and invalidates parse memos. Slang state is lexical to the
  unit: importers and EVAL strings stay in the stock grammar (pinned in
  `t/slang-tuxic-activation.t`, rakudo-verified against the real upstream
  dists).

Also new: generic `.^mixin(R)` on types/values (it previously died with "No
such method 'mixin'"), routed through the same composition as infix `but`.

Not covered yet: Slangify's own upstream test activates a fixture slang
overriding `identifier`/`name` (trailing `?`/`!` on identifiers) — those
rules are not in the map, so it fails loudly per the ADR
(`todo/tickets/slang-piersing-identifier-name-overrides.md`). Next:
re-measure `use Text::CSV` and its 33-file suite
(`docs/batteries/csv.md`).
