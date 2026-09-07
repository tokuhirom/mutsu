# A `<subrule>` called from a ratcheted caller is walked once, not enumerated

ADR-0073's Slice 2 — the `<subrule>` boundary, the last collect-then-pick
barrier in the regex engine — is closed for its **ratcheted** half.

`token` and `rule` declarators are ratcheted by definition, so a caller written
with either cannot backtrack into the subrule it calls: only the subrule's
highest-priority end can ever be used. The eager producer computed every end
anyway and then dropped all but that one. Because an embedded `{ … }` block runs
inline for real where the cursor reaches it (ADR-0009), that fired the block once
per end *computed* instead of once per end *entered*:

```raku
my $n = 0;
grammar G { regex part { \w* { $n++ } }; token TOP { <part> 'c' } }
G.parse("abc");
say $n;                 # raku: 1     mutsu before: 5
```

The parse fails in both implementations, and for the same reason: `\w*` eats the
trailing `c`, the ratcheted caller's literal then has nothing left, and there is
no second candidate to fall back to. mutsu already agreed on that verdict — it
just paid for four candidates it could never use, and ran the user's block four
extra times to do so.

## What changed

`regex_match_atom_all_with_capture_opts` (`src/runtime/regex/regex_match_atom.rs`)
is the eager producer with one new knob, `subrule_first_only`, fed straight from
the calling token's `ratchet` flag by `for_each_atom_candidate`
(`regex_match_lazy.rs`). When it is set, the `Named` arm walks each resolved
candidate body with `first_only` — `regex_match_end_from_caps_in_pkg` instead of
`regex_match_ends_from_caps_in_pkg` — so the walk stops at the highest-priority
complete match. The surviving candidate is provably the same one the old path
kept (the dedup keeps the first end per position, and the atom driver then
drained everything but the highest-priority entry); the difference is only that
the discarded ends are no longer computed.

## The guard is the load-bearing part

The `Named` arm carries the left-recursion growing-seed loop (`LR_ACTIVE` /
`LR_MEMO` / `LR_SEED_READ`), which decides whether a rule is left-recursive *at
this position* by evaluating its candidates and then asking whether the seed was
consulted. A `first_only` walk stops at the first complete match, so it can
return before ever entering the branch that re-enters the rule — and the loop
would then conclude "not left-recursive" and keep an ungrown seed.

That is measured, not hypothetical. In

```raku
grammar G { token TOP { <expr> }; token expr { <term> | <expr> '+' <term> }; token term { \d+ } }
```

LTM ranks `<term>` ahead of the recursive branch, so an unguarded `first_only`
walk of `expr` stops on `1` and `G.parse('1+2+3')` fails.

The new `src/runtime/regex/regex_subrule_lazy.rs` holds the sound precondition
that rules the hazard out: `pattern_is_rule_call_free` — a body that cannot
invoke a named rule at all cannot re-enter *itself*, so its seed can never be
consulted and the growing loop is guaranteed to stop after one iteration whether
or not the walk was cut short. The predicate lists the safe `RegexAtom` variants
explicitly and answers `false` for anything else, so a newly added variant is
excluded until somebody has thought about it; `<name>`, `<.ws>`, `<{ … }>` and
`<~~>` are the excluded four. The seed loop also keeps a runtime fallback (redo
the iteration with the full candidate set) for the exotic case of a `{ … }`
block re-entering the rule by hand.

The precondition is an over-approximation — it admits leaf rules only, so a
grammar's interior rules still compute their full end set under a ratcheted
caller. Tightening it to "this rule is not part of a call cycle" needs a
rule-call-graph analysis and stays as residue in
`todo/deep/ordered-alternation-eager-candidate-enumeration.md`, together with
the non-ratcheted (`regex`) caller, which really can backtrack into the subrule
and therefore needs a *streamed* candidate set rather than a truncated one.

## Measured

`B` is an embedded block incrementing a counter; the cells are how many times it
ran. Every row was run against `raku` (2026-09-07) and a fresh `cargo build`.

| # | shape | subject | raku | before | after |
| --- | --- | --- | --- | --- | --- |
| E1 | `regex TOP { <part> 'c' }` / `regex part { \w* {B} }` | `aaac` | 2 | 5 | 5 |
| E2 | `regex TOP { <a> 'c' }` / `regex a { <b> }` / `regex b { \w* {B} }` | `aaac` | 2 | 5 | 5 |
| **E3** | `token TOP { <part> 'c' }` / `regex part { \w* {B} }` | `aaac` | **1** | **5** | **1** |
| E3b | `token TOP { <part> 'a' }` / `regex part { \w* {B} }` | `aaa` | 1 | 4 | 1 |
| E6 | `regex part { 'a' [ 'b' {one} \|\| 'bc' {two} ] }` under `regex TOP { <part> 'cd' }` | `abcd` | `one` | `one,two` | `one,two` |

Controls, all unchanged and all matching raku: E4 (`regex` caller, `token`
subrule) 1; E5 (`token`/`token`) 1; E7 (a `$*`-declaring caller) 1; E8 (a
`make`-bearing block) 1; E3f (a proto subrule under a ratcheted caller) 1; E3g
(a quantified `<part>+` under a ratcheted caller) 4. The match verdicts are
unchanged too — every one of these shapes fails to parse in raku and in mutsu,
before and after.

`t/regex-lazy-candidate-enumeration.t` grew from 80 to 87 rows: E3's `todo` is
gone, the parse-verdict half of E3/E3b is pinned alongside the counts, and the
two left-recursion rows (E3d/E3e) pin the guard. Those two have no `raku` oracle
— Rakudo has no growing-seed loop and hangs on a left-recursive rule — so they
pin a mutsu capability against exactly the regression an unguarded Slice 2 would
have caused.
