# ADR-0022 Slice 5: non-constant `$var` interpolation no longer participates in LTM ranking

Implemented the final slice of ADR-0022 (`docs/adr/0022-regex-alternation-ltm-ranking.md`):
a `|` alternation branch whose text comes from interpolating a *runtime variable* (`my $x = ...`)
into the pattern no longer competes on declarative-prefix length or litlen the way a literal
written directly in the source does. A branch interpolated from a `constant`-declared value
still participates normally, matching Rakudo (which inlines a `constant`'s value as a literal
at compile time, but cannot do the same for an ordinary `my`/`state`/parameter scalar whose
value is only known at match time).

This closes the last failure in `roast/S05-metasyntax/longest-alternative.t` (test 50):

```raku
constant $x = 'ab';
is ~('ab' ~~ / a | b | $x /), 'ab';  # constant participates: longest branch wins

my $y = 'ab';
is ~('ab' ~~ / a | b | $y /), 'a';  # non-constant: does not count toward LTM
```

The file is now fully green (62/62, whitelisted) — the only remaining gap is the documented
`#?rakudo todo` negative-lookahead LTM quirk on line 461, where mutsu deliberately matches
Rakudo's own current (buggy) behavior.

## Implementation

The actual `$var`-in-regex substitution for the general (non-`:my`-declared) case turned out
to be `Interpreter::interpolate_regex_scalars` (`src/runtime/regex_parse_modifier.rs`), called
unconditionally at the top of `parse_regex_uncached` for every `Match`-mode parse — not
`interpolate_bound_regex_scalars` as the ADR's Slice 5 sketch named (that function handles a
narrower, closure-capture-driven case). Two pieces were needed:

1. **A runtime-visible "is this a `constant`" marker.** The compiler already tracks
   `constant_vars_in_scope` for a different purpose (ADR-0006 inlining), but that is
   compile-time-only state with no way to reach `interpolate_regex_scalars`, which runs on a
   bare `&self` long after compilation. `exec_set_local_op_inner`
   (`src/vm/vm_var_assign_set_local.rs`) now writes a companion env key
   (`__mutsu_constant_var::<name>`) whenever a scalar `constant` is declared, and clears it on
   an ordinary `my`/`state` declaration of the same name — the same "companion marker key in
   the same env map" convention already used for `__mutsu_sigilless_readonly::`, so it inherits
   the env's existing overlay/tombstone scoping (a `my $x` shadowing an outer `constant $x`
   correctly un-marks it) for free.
2. **Threading the "non-declarative" fact from the interpolator to the tokenizer.** Per the
   ADR's own "acceptable interim" for this slice, `interpolate_regex_scalars` wraps a
   substituted span with a reserved control-character sentinel
   (`NON_DECLARATIVE_INTERP_MARK = '\u{1}'`) when the source name is not a compile-time
   constant. The structural tokenizer in `parse_regex_uncached` (`regex_parse_core.rs`) toggles
   a `bool` on that sentinel and tags every `RegexToken` built while it is set with a new
   `from_runtime_interpolation` field. `walk_tokens` (`regex_match_core.rs`) and
   `ltm_litlen_walk` (`regex_ltm_rank.rs`) then treat such a token as an LTM `Terminate`
   stopper, exactly like a code block or `<.ws>`.

`RegexToken` gained the new field at all ~40 construction sites (mechanical, no new bindings
before this slice — every existing token still defaults to `false`).

## A pre-existing bug this slice exposed and fixed

Making `<&subrule>`-style interpolated literals sometimes terminate under `LTM_DECLARATIVE_MODE`
surfaced a latent bug: `regex_match_with_captures`'s `:let`/`:temp`/`:my`/`:constant`
declarative-prefix handling (`regex_match_public.rs`) evaluates its initializer as real code
with a real env write, unconditionally — including when this function is reached from *inside*
an LTM measurement (`declarative_prefix_match_len` → `regex_match_len_at_start` →
`regex_match_with_captures`, used by the "anchored single subrule" ranking path). Before this
slice, a measured subrule's literal content was always compared for real against the subject,
so the measurement's own success/failure matched reality and `:let`'s restore-on-fail logic
never had a reason to misfire. Slice 5's new zero-width termination broke that invariant: a
`:let $a = 5; <&lma>` pattern being *measured* (not really matched) could now report a
spurious zero-width "success", making the wrapper's `matched = result.is_some()` check skip
restoring `$a`, permanently leaking the `:let` value into the caller's scope.

Fixed by making `regex_match_with_captures` check `LTM_DECLARATIVE_MODE` before applying any
declarator: under measurement, it now skips the declarators (which are zero-width and
consume no input, so this changes no measured length) and measures only the remaining
pattern — restoring the ADR-0009 "measurement never executes user code" discipline this
mechanism had always been violating, just never observably before. Pinned by the existing
`t/regex-declarative-modifiers.t`.

## Also fixed: sentinel leaking inside double-quoted regex literals

The sentinel character, when the interpolation happened inside a double-quoted regex
literal (`"$var..."`), leaked past the tokenizer's own quote-scanning inner loop (which reads
characters directly, bypassing the main token loop that strips the sentinel), corrupting the
match. Scoped down: `interpolate_regex_scalars` now skips sentinel-marking when
`is_inside_double_quoted_regex_literal` is true, leaving such interpolations declarative
(unchanged from Slices 1-4) rather than breaking them. A `// TODO:` marks this as a narrower
scope than the general case — teaching the double-quoted-literal tokenizer arm to also honor
the sentinel is left for a future slice; it is not required by any roast test.

Pinned locally by `t/regex-ltm-alternation.t` (extended with four new cases) and
`t/regex-declarative-modifiers.t`/`t/regex-interp-method-call.t` (regression guards).
