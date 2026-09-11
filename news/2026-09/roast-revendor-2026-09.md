# roast re-vendored to 85a87909 (2026-09-07)

`roast/` had been pinned at `b2cbe8a4` (2026-06-12) for three months. It is now re-vendored at
upstream master `85a87909` (2026-09-07) via `scripts/update-vendor.sh roast`, which also rewrote
the `roast` row of `vendor.lock`.

## What moved upstream

Seventeen commits, touching 20 test files plus `spectest.data`, and adding one brand-new file.
They fall into three groups.

### 1. Statistical de-flaking — the reason this update pays for itself immediately

Upstream raised the `.roll` sample size in the Set/Bag/Mix tests from 100 to 100000 and scaled the
assertion bounds with it ("Make more statistical tests less likely to fail"). That removes the
entire class of failure mutsu had been quarantining since July:

- `roast/S02-types/bag.t`
- `roast/S02-types/baghash.t`
- `roast/S02-types/mixhash.t`

All three bounded a Binomial(100, 1/3) sample (`2 < a < 75`, `a + 2 < b`), which a *correct* RNG
violates with small but real probability — measured at roughly 0.1% per run for `bag.t` test 117.
With n = 100000 the same assertions sit about 100 standard deviations from the mean, so they cannot
fail by chance. All three entries were removed from `flaky-tests.txt` (3/3 green locally, ~1-2s each
on the release binary), leaving `roast/integration/advent2014-day05.t` as the single remaining
quarantined test. The long prose explanations in `CLAUDE.md` moved from the "known flaky" list to
the "de-flaked" list: a failure in those three files is a real bug again.

Other de-flaking in the same batch, all of which mutsu passes unchanged: `S17-channel/basic.t` now
awaits the Supply's `done` before reading what the tap saw, `S17-promise/then.t` awaits the
independent `.then` it had been reading without awaiting, `S29-os/system.t` runs a silent child
instead of `$*EXECUTABLE -v` in its 200-iteration loops, and `S32-io/IO-Socket-Async.t` holds a real
port for its EADDRINUSE probe instead of guessing a "hopefully invalid" one.

### 2. Tests rewritten so the runtime, not the compiler, does the rejecting

`6.c/S04-declarations/my-6c.t`, `S04-declarations/my-6e.t`, `S09-typed-arrays/arrays.t` and
`S12-enums/basic.t` now assign a value held in a variable (`my $str = "str"; my Int $a; $a = $str`)
rather than a literal, so the declared type is what rejects it rather than a constant-folding
compiler. `S03-operators/short-circuit.t` judges the `xor` precedence warning by its own line, and
`S05-substitution/subst.t` splits an `ss:i:m` case that was conflating marks into two. mutsu passes
all of these unchanged.

### 3. New spec tests for rakudo bugs that are still open

The remaining commits add subtests for five unfixed rakudo issues — capture state after
backtracking (rakudo#4105), block-quantifier limits under backtracking (#5588), character-class
fold and mark semantics (#2962), lookaheads over compound character classes (#4512), and C99
hexadecimal float literals (#6524) — plus a new 586-subtest file for the `%a`/`%A` `sprintf`
directives.

These are spec tests written *ahead of* the implementation, so **the local oracle fails them too**.
Measured against Rakudo v2026.07:

| File | mutsu | raku v2026.07 |
|---|---|---|
| `S05-capture/caps.t` | 55/56 | 43/56 |
| `S05-metasyntax/charset.t` | 89/90 | 57/90 |
| `S05-metasyntax/regex.t` | 66/68 | 58/68 |
| `S05-modifier/ignorecase.t` | 110/115 | 103/115 |
| `S02-literals/numeric.t` | parse error (0/89) | SORRY |
| `S32-str/sprintf-a.t` | 0/586 | 0/586 |

mutsu is *ahead of* the local rakudo on all four regex files and level with it on the two hexfloat
ones. Nothing regressed — the target moved.

## Whitelist follow-up

Per `docs/vendoring.md`, every whitelisted file upstream touched was re-run. Thirteen of the
nineteen still pass. The five that no longer pass completely were removed from
`roast-whitelist.txt` (1436 → 1431) and recorded in `TODO_roast/BLOCKERS.md` under "Files
dewhitelisted by the 2026-09-11 roast re-vendor", classified **No oracle (spec ahead of the local
rakudo)** with the exact failing subtest numbers and what each one asserts. The new
`S32-str/sprintf-a.t` is listed there too.

Four of the five miss the whitelist by one or two subtests, so they are close: `caps.t` needs only
test 47, `charset.t` only test 54, `regex.t` only 59 and 67. `numeric.t` is the outlier — a single
parse error on `0x1.8P4` kills all 89 subtests, so implementing the C99 hexfloat literal in the
lexer recovers a whole file at once and would put mutsu ahead of rakudo v2026.07 on it.

`S32-io/IO-Socket-Async.t` stays whitelisted: it times out at "planned 40 ran 17" in the remote
agent container, which is the documented network-sandbox failure (see `docs/agent-environments.md`),
not a consequence of the upstream change — it reaches the IPv6 section long before the rewritten
EADDRINUSE probe.
