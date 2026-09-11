# A skill for taking one zef distribution from red to green

`ecosystem/` measures how mutsu does against rakudo on real zef distributions, and
`docs/ecosystem-parity.md` says how the measurement is made — but nothing said what to *do* with a
record that reads `red` or `blocked_load`. Acting on one was reconstructed from scratch each time:
the sweep extracts into a temporary directory and deletes it, which is exactly right for a
measurement and useless for the thirty-runs-and-a-rebuild loop that fixing a distribution actually
is.

`.agents/skills/ecosystem-dist-fix/` is that loop, written down. A request like "make
`String::Utils`'s tests pass" now has one procedure: read the ledger record to learn what kind of
job it is, check the distribution out, run every test file under rakudo *first* and mutsu second,
reduce the failure to a few lines that live in this repo, fix the interpreter or file an issue, pin
the fix with a `t/` test, re-measure, and land the record change alongside the fix.

Two parts of it are worth calling out.

**The fix-versus-issue rule is explicit.** A distribution routinely yields two bounded fixes and one
finding that needs an ADR, `nqp::` guts, the MOP or a cross-cutting invariant. The skill makes that
a per-finding decision rather than a per-distribution one: fix what is bounded, file the rest as
`todo:ticket` / `todo:deep` with the reduction and both interpreters' output in the body, title it
after the missing capability rather than the distribution, and name the issue numbers in the PR. A
distribution that goes from `red` to `partial` with its residue filed is a successful run; a finding
left unrecorded because it was too big to fix is the one way the loop loses work.

**`checkout-dist.py` gives the loop somewhere to stand.** It resolves the same flat dependency
closure the sweep does, extracts the distribution and every dependency under `tmp/ecosystem/`
(gitignored, reused across runs), and prints `DIST` / `LIBS` / `MUTSU` / `RAKU` plus the test file
list, so both sides are guaranteed to run with the same `-I` list and the same working directory —
the fairness contract the comparison depends on. It accepts a module name and says which
distribution provides it, warns when the closure is incomplete instead of silently under-resolving,
and deliberately does not sandbox: a single distribution you have read is the sweep's own
`--sandbox none` escape hatch, and a corpus is not this skill's business.

The skill also records the ground rules that make a green run mean something, several of which are
easy to violate by accident: rakudo is the denominator and gets run first, `MUTSU_FUDGE` stays
unset, the extracted distribution is never edited into a deliverable, mutsu never learns a
distribution's name, dependencies arrive only as `-I` paths, and bundling the module instead of
fixing the interpreter remains banned. One drift was found while writing it and is noted where it
would otherwise cost an hour: `docs/ecosystem-parity.md` §5 documents `--mutsu-only`,
`--refresh-baseline` and `--json -`, which the harness does not implement.
