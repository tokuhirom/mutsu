# JSON::Fast decode: fewer instructions per string

The SPDX-shaped `from-json` benchmark (`bench-json-fast-spdx@section`, #8673)
read 1.17-1.35x rakudo on the last `main` rows. Three changes take about
7% off a 100-record decode (53.0M instructions before; callgrind, warm
precompilation cache, `DECODES=2` minus `DECODES=1`). A fourth slice, a
step-by-one peephole fusion, was superseded by the more general
constant-add fusion that landed on `main` in parallel (c8124bcb2).

- **The `Uni` copy loop.** `unjsonify-string` copies every character of an
  escaped string with one shift and one push. Both ops now have arms of
  their own in the TRIR loop instead of a second dispatch through
  `trir_list_op`; an output `Uni` starts at 32 slots instead of reallocating
  at 4, 8, 16 and 32; `strtocodes` fills its store in one allocation; and
  `strfromcodes` turns an all-ASCII store into its bytes directly. (-3.3%)
- **The `ordat` string memo hits by identity**, a pointer compare instead
  of reading both strings. (-0.7%)
- **`Uni:D \codes` is checked from the type tables.** A bare `Uni` against
  `Uni` or its own form is decided without the scope-dependent name
  resolution walk when nothing can rebind the name (no type capture bound,
  no package alias, no subset), and the TRIR call no longer materializes
  its deferred frames before such a check. (-3.1%)

The goal of #8673, a ratio under 1.0, is not reached by these alone; the
bench CI rows after the merge give the new standing.
