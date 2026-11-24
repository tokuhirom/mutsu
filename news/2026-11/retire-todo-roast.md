# The `TODO_roast/` blocker ledger is retired

Roast has turned from a work source into a regression guard: 1440 of the 1455
roast `.t` files are whitelisted, and every one of the remaining 15 is a
non-goal (rakudo fails it too, or the test is broken upstream), has no oracle,
or cannot pass. The `TODO_roast/BLOCKERS.md` ledger, which once ranked
hundreds of failing files by root cause, had shrunk to a single table of those
15 files.

That table now lives in `docs/roast-compatibility.md`, under "Non-whitelisted
roast files and why", next to the definition of the headline roast figure. The
two investigation notes that were still live (`S06-advanced/caller.t` and
`S12-traits/basic.t`) were folded into their rows. The data files moved to
`docs/data/`: the roast × raku baseline (`raku-baseline.tsv` and its writeup,
written by `scripts/roast-raku-baseline.sh`) and the ADR-0029 `X::` exception
role-membership capture (written by `scripts/adr0029-capture-x-exception-data.py`).

`AGENTS.md` no longer lists roast in the task-selection order: work comes from
`PLAN.md`, the issue queue by lane and the ecosystem's failing-dist queue, and
the `roast-triage` skill now covers investigating a failing roast file rather
than choosing the next one. Every other reference, including source comments
that pointed at ledger sections, now names the roast file or the new table.
