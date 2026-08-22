# Proto-token dispatch now shares the one LTM ranking mechanism

[ADR-0046](../../docs/adr/0046-proto-token-ltm-shares-one-ranking-mechanism.md)
is fully implemented. mutsu had **three** Longest-Token-Matching ranking
implementations with three different algorithms; it now has one, used at all
three call sites, and interpolation provenance reaches every interpolation
site instead of just `$`-scalars in the general-case interpolator.

## What was wrong

| # | site | algorithm before |
|---|---|---|
| 1 | `dispatch.rs::eval_token_call_values_at` (`:rule<...>` / outermost proto entry) | declarative **prefix length** only, decl-order tie-break |
| 2 | `regex_match_atom.rs::ltm_rank_and_collect_branches` (`\|` alternation, ADR-0022) | `(prefix_len, litlen, decl order)` |
| 3 | the nested-`<name>` proto candidate loop in `regex_match_atom.rs` | **longest actual match end**, decl-order tie-break |

Mechanism 3 is the one every nested subrule call goes through — essentially
every real grammar. It never set `LTM_DECLARATIVE_MODE` at all: it ran the real
matcher on *every* candidate and sorted by end. That is the pre-ADR-0009
algorithm, and it was invisible to `roast/S05-grammar/protoregex.t`, whose LTM
assertions all use `subparse(:rule(...))` and so enter through mechanism 1.

Separately, the `NON_DECLARATIVE_INTERP_MARK` provenance sentinel ADR-0022
Slice 5 introduced had been wired into exactly one of mutsu's interpolation
sites, so array interpolation and grammar/token-body interpolation both
produced structure indistinguishable from hand-written literals.

The headline repro — `token val:sym<known> { :i 'Foo=' @opts }` racing
`token val:sym<other> { <-[;]>+ }` inside `token TOP { … <val> … }` — answered
`KNOWN` where `raku` answers `OTHER`, and needed fixes on both axes to flip.

## What landed

- **Slice 1** — array/regex-object interpolation provenance (`@name`, `@$var`,
  `@(...)`, `<@var>`, `<$var>`). Every array/regex-object form terminates the
  declarative prefix unconditionally, including a `constant @a`; the `$`-scalar
  `constant` exemption has no `@` analogue.
- **Slice 2** — the same marking in `interpolate_bound_regex_scalars`, the
  substitution pass that renders grammar/token bodies.
- **Slice 3** — mechanism 1 onto `ltm_branch_rank_key`, gaining the `litlen`
  tie-break. This forced `LtmAtomMode::SkipZeroWidth` for `RegexAtom::VarDecl`:
  measuring a parsed pattern no longer goes through the string-level declarator
  strip in `regex_match_with_captures`, and measuring a `:my` for real
  *executes* its initializer. Fixing that at the atom level also closed a real
  leak the string-level guard missed — measuring
  `token TOP { :my $shared = 'zz'; <inner> }` wrote `$shared` into the live env,
  which `inner`'s own pre-substitution then read.
- **Slice 4** — mechanism 3 restructured from "match everything, sort by end" to
  "rank by measurement, then match the winner". Because ranking runs under
  `LTM_DECLARATIVE_MODE` it executes nothing, so a losing proto candidate's
  `{ }` blocks and action methods no longer fire (ADR-0046 §2.3). The
  left-recursion growing-seed loop falls back to evaluating the full candidate
  set once it observes the seed was consulted, so a skipped candidate cannot
  hide a re-entry.
- **Slice 5** — this entry, plus the ADR's status line.

Three corrections Slice 4 forced along the way (each validated against `raku`
before implementing) are recorded in the ADR's Slice 4 notes: `<sym>` is a
*named capture*, not a bare literal splice; a character class written with set
subtraction terminates the prefix (see
`news/2026-08/subtracted-char-class-terminates-ltm-prefix.md`); and the
`X || Y` epsilon bypass makes a `None` measurement unsound to *filter* on.

## Verification

`t/regex-ltm-proto-dispatch.t` (35 assertions) and
`t/regex-ltm-interpolation-provenance.t` (20) pin ADR-0046 §2.1, §2.2 and §2.3;
every assertion produces identical TAP under `raku` and under mutsu. Full `t/`
(3346 files), `make roast` (1436 files, 218836 tests) and the bundled-battery
gate all pass.

One lesson worth keeping: Slice 4's `||`-epsilon unsoundness passed the entire
`t/` suite *and* all of roast, and was caught only by
`scripts/battery-testsuite.sh` (Cro::Core's `uri.rakutest`, whose IPv6 host
candidate is one big `||` chain). LTM changes must be run against the battery
gate locally before pushing.
