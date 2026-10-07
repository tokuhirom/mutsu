# The method table is built without rescanning every row per shape

The built-in method table (ADR-11276) is built by the first method call of each process. Its shape
pass visited every row once per (shape, MRO owner) pair and compared owner strings, so the build cost
grew with the row count times the number of shapes: each method-row slice added ~1.7M instructions to
any script that called a method (`string-concat` +27%, `bench-grammar-parse` +21%, #12195).

The build now groups rows by owner once, interns each row's name once, and interns the owner once per
run of rows. Deterministic instruction counts (`scripts/bench-det.sh`, warm, JIT off):
`string-concat` 25.1M (was 31.5M at `b41bdf46`, 26.1M before the slices) and `bench-grammar-parse`
30.6M (was 34.8M, 29.5M before the slices).
