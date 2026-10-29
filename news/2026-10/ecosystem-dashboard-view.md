# Ecosystem dashboard: the work queue as one generated issue body

`scripts/ecosystem-dashboard.py` renders the ecosystem work queue the way Renovate's Dependency
Dashboard does: a bulleted list of distribution names and issue numbers, in three parts (locked,
waiting on an issue, free queue) plus a "Needs attention" part.

The dashboard is a read-only view. Locks stay append-only comments on the lock board, because a
hand-edited issue body is a read-modify-write that two parallel agents can both win. The body is
rewritten wholesale (`--update <N>`), so a rerun cannot lose anyone's edit.

A distribution is linked to an issue by a marker line in the issue's body, `eco-dist: A::B, C::D`,
in the same style as the existing `eco-cluster: <id>` line (which now also counts: it expands to
every distribution of the cluster). The ledger records carry no issue numbers, so the nightly
sweep stays the only writer of the `ecosystem-data` branch. The `ecosystem-dist-queue`,
`ecosystem-dist-fix` and `ecosystem-cluster-filing` skills now say to write the marker when
filing, and the dashboard lists open `ecosystem` issues that mention a queued distribution without
one.
