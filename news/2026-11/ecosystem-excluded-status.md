# Ecosystem page shows `excluded` distributions and leaves them out of the KPI

`ecosystem/exclude.txt` only steered the sweep's bulk selection and the work
queue; the published page ignored it, so an excluded distribution (for example
the `MoarVM::` namespace) either stayed in the parity denominator as a failure or
vanished from the table depending on whether it had been measured earlier.
`scripts/gen-ecosystem-manifest.py` now marks every listed distribution with the
status `excluded`, carries the list's reason into the "Why" column and drops it
from the graded set, the same way `no_baseline` is treated. The page gains an
"out of scope" / 「対象外」 badge and filter entry.
