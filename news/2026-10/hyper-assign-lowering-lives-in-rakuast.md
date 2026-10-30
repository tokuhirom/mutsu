# The hyper-assignment lowering lives in `rakuast`

ADR-15334 (reading B), hyper-assignment family. The lowering of `TARGET »=» VALUE` and its dwim spellings
(`«=«`, `»=«`, `«=»`) -- an ordinary hyper op for a plain target, the element-wise destructuring of a literal
list of lvalues (nested sublists included) through the shape and distribution temporaries otherwise -- moved
from `src/parser/expr/precedence_meta_ops/hyper_concat.rs` to `src/rakuast/hyper_assign.rs`. The parser keeps
the parse functions and the typed-node construction (`written_hyper_assignment`) and calls the one lowering;
`rakuast::lower` calls it directly, so the `parser::hyper_assignment_expr` re-export is gone.

`t/rakuast/rakuast-stage2-hyper-assign-paths.t` pins that a file, an `EVAL` string and an `EVAL` of the
string's `.AST` mean the same thing for these forms (declared inside the unit and in the caller); it passes
identically under `raku`. `@a «=« (9, 8)` keeps one element fewer than rakudo before and after this change
([#15527](https://github.com/tokuhirom/mutsu/issues/15527)). The `internal-name-literals` ceiling drops 61
to 59; the two hand-rolled walkers moved with their code.
