# The RakuAST frontend is a checked layer

`src/rakuast` named the runtime and the compiler in ten places, which blocked lowering where the
parser builds a node (ADR-15760 §2.1). Nine of them were pure tables and are now below the
frontend: the operator-name tables (`compiler::helpers_ops`, now `src/op_names.rs`, still
re-exported under the old path), `is_known_type_constraint` (`src/known_types.rs`), the builtin
function names (`src/builtin_functions.rs`), and callers of `term_names` and `format_num_raku`
now name the leaf modules they already lived in.

`rakuast` joined the lower layers of `scripts/check-layer-deps.py`. Its one remaining upward
reference is the slang activation hook in `rakuast/mod.rs`, which runs code and stays in the
baseline until it goes through a trait.
