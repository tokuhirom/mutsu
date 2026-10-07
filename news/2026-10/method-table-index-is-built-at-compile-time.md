# The method table's lookup index is built at compile time

The first method call of every process used to build the built-in method table (ADR-11276): it
interned every row's name and hashed every `(receiver, name, arity)` key, so each method-row slice
cost any script that called a method another ~1.7M instructions (#12195).

`table_const.rs` now evaluates that index in `const` context: the rows flattened in registration
order, the distinct names and owners sorted, and the first-wins `(receiver, arity) -> row` entries
resolved along each shape's MRO, together with the per-name arity and shape bit masks. A `Symbol` is
interned at run time, so it maps to its name index lazily: one binary search per distinct method name
per thread, memoised by symbol id. Adding rows now costs compile time, not start-up time.

The run-time builder it replaced is kept as the oracle in
`method_table/tests/const_index.rs`, which checks every name, shape, receiver kind and arity against
it. `Interpreter::build_builtin_registry` aside, the first `.chars` call now costs 0.33M instructions
over `say 1` (was 2.0M). Deterministic counts (`scripts/bench-det.sh`, warm, JIT off):
`string-concat` 23.5M and `bench-grammar-parse` 28.8M, both below their pre-slice values (26.1M and
29.5M).
