# The bench trend notes that rakupp parses JSON::Fast natively

`bench-json-fast-spdx@section` reads about 17x against rakupp while it reads
1.2x against Rakudo. The gap is not an engine gap: rakupp does not run
JSON::Fast's Raku code at all. It loads the module file, but answers
`JSON::Fast::from-json` with its own C++ JSON parser -- a `die` put at the top
of `from-json` in a copy of `modules/JSON-Fast` never fires under rakupp
v5.0.1, while the call still returns the parsed data. So that row compares
mutsu interpreting JSON::Fast with a native parser, about 150 MB/s on the
340 KB SPDX document.

The dashboard (`scripts/bench-visualize.py`) can now attach a caveat to a
benchmark for one metric (`BENCH_NOTES`). The card shows it under its readout
while that metric is selected, and the table view marks the row with a `†`
carrying the note as a tooltip. `bench-json-fast`, `bench-json-fast-spdx` and
`bench-json-fast-spdx@section` carry this one under **ratio vs rakupp**, and
point at **ratio vs rakudo** as the comparable number.
