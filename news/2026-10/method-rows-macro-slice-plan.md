# The method-table migration is re-cut into nine PRs

ADR-11276 migrated built-in methods into handler rows one family per PR. In three days that was more
than twenty PRs for about 190 of the 1,648 recognition rows. A PR that touches code costs the same CI
run (a release build and the whole TAP and roast suites, about 22 runner-minutes) whatever its diff,
so the remaining 1,479 rows would have been about 180 more runs.

The ADR now has a slice plan (§10). A PR is justified by a mechanism or by an owner group's
deletion unit, never by a family. Slice 3A builds the guard for every built-in receiver kind, the
interpreter handler kind and the group scaffolding once. Slices 3B to 3G then move the numbers and
text, the collections, the instance classes, I/O and concurrency, the receiver-mutating methods, and
the constructors and metaobject protocol, each as one branch of per-family commits and one PR.
Slice 4 is the resolver cutover and slice 5 the deletion. After 3A the six group slices need nothing
from each other, so they can run in parallel.

Until 3A merges, no PR migrates a family. The issue body of #11276 carries the live status of each
slice.
