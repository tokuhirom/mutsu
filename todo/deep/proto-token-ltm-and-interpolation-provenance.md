# Proto-token LTM: nested `<name>` dispatch never measures a declarative prefix, and interpolation provenance stops at `$`-scalars

> **Design complete (2026-08-20): see [ADR-0046](../../docs/adr/0046-proto-token-ltm-shares-one-ranking-mechanism.md)**
> for the validated Rakudo semantics, the probe matrix, the five implementation slices,
> and the rejected alternatives.
>
> **The root cause recorded in the original version of this file was wrong.** It is kept
> below (§"Superseded root-cause theory") only so nobody re-derives it. Do **not** start by
> building static quantifier-boundedness analysis.

## Symptom (verified against `raku` on `main` @ `4e4ca30ce`, 2026-08-20)

```raku
grammar G {
    my @opts = <Strict Lax None>;
    token TOP { <name> [';' ' '? <val> ]* }
    token name { <-[;]>+ }
    proto token val {*}
    token val:sym<known> { :i 'Foo=' @opts }
    token val:sym<other> { <-[;]>+ }
}
class A {
    method val:sym<known>($/) { make "KNOWN" }
    method val:sym<other>($/) { make "OTHER" }
}
say G.parse('x; Foo=Strict', :actions(A.new))<val>[0].made;
# raku: OTHER    mutsu: KNOWN
```

## Actual root cause (two independent gaps)

**Gap A — mutsu has three LTM ranking implementations, and the one every nested subrule
call uses has no declarative-prefix measurement at all.** The proto-candidate loop in
`src/runtime/regex/regex_match_atom.rs:541-599` runs `regex_match_ends_from_caps_in_pkg`
on *every* candidate for real and sorts by longest actual end, decl-order on a tie. It
never sets `LTM_DECLARATIVE_MODE`, has no litlen tie-break, and executes losing
candidates' code blocks and action methods. `roast/S05-grammar/protoregex.t` misses this
entirely because all of its LTM assertions use `subparse(:rule(...))`, which enters
through the *other* proto mechanism (`dispatch.rs::eval_token_call_values_at`). Minimal
proof — a plain `{ }` code block, the oldest stopper mutsu implements (ADR-0009):

```raku
proto token val {*}
token val:sym<known> { 'Foo=' {} 'Strict' }   # declarative prefix 4
token val:sym<other> { <-[;]>+ }              # declarative prefix 10
# G.parse('Foo=Strict', :rule<val>)  -> raku OTHER, mutsu OTHER   (mechanism 1: correct)
# G.parse('Foo=Strict')  with token TOP { <val> }
#                        -> raku OTHER, mutsu KNOWN               (mechanism 3: broken)
```

**Gap B — ADR-0022 Slice 5's interpolation provenance was wired into exactly one of
mutsu's interpolation sites.** `RegexToken::from_runtime_interpolation` (checked in
`walk_tokens`, `regex_match_core.rs:483`) is set only for `$`-scalar substitutions made by
`interpolate_regex_scalars`. It is *not* set for:

- array interpolation — `push_regex_interpolated_alternation`
  (`regex_parse_modifier.rs:612/668/704`) splices the elements into the pattern *text* as a
  literal alternation, so the interpolation boundary is gone before anything measures it;
- the `<@var>` / `<$var>` assertion forms — `array_var_alternation_atom`
  (`regex_parse_core.rs:351`) builds a plain `RegexAtom::Alternation`;
- grammar/token bodies — `interpolate_bound_regex_scalars` (`regex_interpolate.rs:288`,
  reached via `eval_token_def`) substitutes `$var` with no marking at all.

Rakudo terminates the declarative prefix at *every* array interpolation form,
unconditionally — including a `constant @a` and an array of `Regex` objects (ADR-0046 §2.1
probes I/J/K/M/Q/R/S). A hand-written literal alternation, which is exactly what mutsu
produces, correctly participates (probe L) — so mutsu is not merely over-eager, it has
erased the distinction.

Both gaps must be fixed for the headline repro to flip: Gap B so `known`'s prefix stops at
4, Gap A so the nested `<val>` dispatch ranks by prefix at all.

## Why this is `deep`

- It is a *unification*, not a patch: three independently-evolved ranking algorithms
  (`dispatch.rs::eval_token_call_values_at`, ADR-0022's `ltm_rank_and_collect_branches`,
  and the `regex_match_atom.rs` proto loop) must collapse onto one primitive, or the next
  LTM refinement gets applied twice out of three times again.
- Restructuring the proto loop from "match everything, sort by end" to "rank by
  measurement, match the winner" changes the dispatch path every grammar in the test suite
  and every battery (Cro, YAMLish, JSON::Tiny, Rakudo-Core) goes through, and interacts
  with the surrounding left-recursion growing-seed loop (`LR_MEMO` / `LR_ACTIVE` /
  `LR_SEED_READ`), which today depends on evaluating all candidates each iteration.
- No roast test pins any of it, so the acceptance suite has to be written from scratch
  (ADR-0046 §5).

## Superseded root-cause theory (do not implement)

The original file attributed the divergence to Rakudo's NFA giving an *inline unbounded*
quantified atom (`<-[;]>+`) structural priority over a *bounded* candidate on a genuine
runtime length tie, and concluded that a faithful fix needed static analysis of each
candidate's quantifier boundedness. There is no tie: `@opts` terminates `known`'s prefix at
4 characters, so `other`'s prefix of 10 wins on plain ADR-0022 rules. The claimed contrast
case — a named-subrule catch-all (`token val:sym<other> { <path> }`) supposedly flipping
Rakudo's preference to `known` — does not reproduce either: `raku` answers `OTHER` for that
shape too.
