# The conjunction arm is demand-driven too

[ADR-0073](../../docs/adr/0073-regex-atom-candidates-are-produced-on-demand.md)
slices 1 and 3 made the group, alternation and separated-quantifier boundaries
continuation-driven: an atom that recurses into a sub-pattern now produces
candidate *k+1* only once candidate *k* has been rejected by the real
continuation, so an embedded `{ ... }` block runs once per candidate the engine
*enters* rather than once per candidate it *computes*. `Conjunction` was left on
the eager producer and recorded as remaining work.

It is the same defect and the same fix:

```raku
my $n = 0;
"aaa" ~~ / ( \w* { $n++ } & \w* ) /;
say $n;        # rakudo: 1     mutsu (before): 4
```

`&` / `&&` require every branch to match the **same** substring, so the atom's
candidates are the first branch's ends that every other branch also reaches. The
eager arm collected the first branch's whole end set before probing any of them,
running the blocks inside it once per computed end.

`drive_conjunction_candidates` walks the first branch through a
`MatchSink::Cont` instead. The other branches keep the eager probe on purpose:
`regex_match_branch_ending_at` asks a yes/no question about **one** end, so
there is no candidate set to stream, and raku evaluates them for the end under
test too. An end that a sibling branch cannot reach is not a candidate at all,
so a `:ratchet` has nothing to commit to there and the walk continues — the
ratchet only fires once a candidate has actually been delivered.

## What was verified, not assumed

Every conjunction shape was run under real `raku` and mutsu side by side, and
all agree:

| shape | result |
|---|---|
| `( \w* & \w* )` on `aaa` | `aaa` |
| `(\w+ & <[a..c]>+)` on `abc` | `abc` |
| `(\d+ & \w+)` on `abc` | no match |
| `(\w+) & (\w+)` on `abc` | `abc`, captures from both sides |
| `( \w+ & \w\w )` on `abcd` | `ab` — the shared end is the shorter branch |
| `^ [ \w+ & 'abc' ] $` on `abc` | `abc` |
| `( \w* {B} & \w* ) b` on `aaab` | matches `aaab`, **`B` runs twice** |

The last row is the one that matters for the fix's soundness: the continuation
rejects the first (longest) end, the walk comes back for the second, and the
block runs exactly once per end actually entered — rakudo's count, not one and
not four.

## Gates

`t/regex-lazy-candidate-enumeration.t` grew rows A16b–A16i and its A16
`todo` marker is gone; the file passes 72/72 under **both** mutsu and `raku`.
`make test` PASS (3764 files, 39314 tests); full local `make roast` PASS.

ADR-0073's Slice 2 (the `<subrule>` boundary, rows E1/E2/E3/E6) is untouched and
remains the last collect-then-pick barrier —
`todo/deep/ordered-alternation-eager-candidate-enumeration.md`.
