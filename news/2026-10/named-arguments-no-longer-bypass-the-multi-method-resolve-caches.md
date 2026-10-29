# Named arguments no longer bypass the multi-method resolve caches

A method call that passed a named argument (`$a.merge($b, $c, :$merge-clones)`)
re-ran the whole candidate walk on every call, because the type-keyed multi
resolve caches refused any `Pair` argument. The method resolve cache and the
`proto method` `{*}` redispatch cache now key a named argument by a reserved
marker, its name and its value's own type key (definedness included); a
candidate set with a `where`, literal, subset, coercion, `is rw` or
sub-signature parameter was already excluded from these caches.

Also, a `proto method` call no longer snapshots and diffs the caller's whole
env overlay: it pins the overlay tier (one `Arc` bump) and compares tier
identity afterwards, so the cost no longer scales with the number of names in
the calling scope (it showed for calls from top-level code).

Measured on a `profiling` build (callgrind, warm, Ir per call, slope of
100 vs 300/400 iterations):

| shape | before | after |
| --- | ---: | ---: |
| `proto` + 2 multi, named arg + return type, called from a method | 81.5k | 59.9k |
| same, called from top-level code | 110.4k | 78.4k (pin only) |
| `ML::TriesWithFrequencies` `trie-create`, 400 words (whole run) | 1,375.9M | 1,343.5M |

The whole-run gain is small (-2.4%): `merge` is ~3 of ~110 method-call opcodes
per word. See #15235 for the per-mechanism breakdown.
