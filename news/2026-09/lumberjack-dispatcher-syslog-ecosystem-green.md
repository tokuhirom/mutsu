# Lumberjack::Dispatcher::Syslog ecosystem record is green

`ecosystem/dists/L/Lumberjack--Dispatcher--Syslog~5f485a33.json` was last measured `partial`
(2/3 baseline files at parity, `t/020-dispatcher.t` failing all 6 assertions with `first_failure:
"trace"`). That measurement predated `edc053cc` ("fix: scope class enum member aliases",
[#8569](https://github.com/tokuhirom/mutsu/pull/8569)), which had already turned the crash on this
file from a `regression` (`X::Redeclaration`-style poisoned-enum-alias die) into a `partial` — the
suite could run, but `t/020-dispatcher.t` still failed at its very first assertion (`trace`).

Re-running the checked-out distribution against current `main` (`73648bdd`) shows all three baseline
files now passing cleanly (confirmed over three repeated runs), so a later, unrelated change closed
the remaining gap. Re-measured with `scripts/ecosystem-sweep.py --only Lumberjack::Dispatcher::Syslog`
and the ledger record now reads `green` (3/3 baseline files at parity, 8/8 assertions).

No interpreter change was needed in this pass — the record was simply stale relative to `main`.
