# Bench CI measures against Raku++ v5.9.0 and records the versions it ran

The bench CI's second reference, Raku++ (`rakupp`), was pinned at v5.0.1 while upstream had
reached v5.9.0. The pin now moves to v5.9.0. The archive's sha256 is pinned in
`bench.yml` too, next to the version, so a download is checked against the archive this
workflow was reviewed with, not only against the checksum published beside it.

`scripts/bench-ci.sh` now prints, as two new trailing columns, the first line of
`--version` of the very `raku` and `rakupp` binaries it measured (`RAKU_BIN`, `RAKUPP_BIN`),
or `NA` when one is missing. The workflow writes those into the history's `rakudo` and
`rakupp` columns, instead of asking whatever is on `PATH` again at append time. bench-trend
reads those columns to mark each reference version change on its ratio charts.

Both rakupp caveats on bench-trend were re-checked for v5.9.0. Its binary is still an ELF
`EXEC` (non-PIE) executable, and it still ships its native JSON routines, so the notes stay.
The non-PIE note's 1.1x figure is now labelled as measured against v5.0.1. The first bench run
on v5.9.0 is the run-time check for the JSON note: if rakupp now ran JSON::Fast's Raku code,
its bench-json-fast time would jump several-fold at the version marker.
