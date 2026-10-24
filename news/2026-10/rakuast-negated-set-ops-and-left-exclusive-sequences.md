# RakuAST keeps negated set operators and left-exclusive sequences as written

`$a ∉ $b` and the other precomposed negated glyphs (`∌ ⊄ ⊅ ⊈ ⊉`) now render as
`ApplyInfix` with their own `Infix`, `$a !(elem) $b` / `$a !∈ $b` as
`MetaInfix::Negate` over the written relation, and `^...`, `^...^`, `^…`, `^…^` as
one `ApplyListInfix` carrying the written spelling (a chain is one flat operand
list), instead of the parser's `not-threaded` / `.skip(1)` desugaring.
Hand-built trees of these shapes lower back to the same execution.

`^…` and `^…^` also parse as sequences now: `^…` used to be read as the junction
`^` followed by `…`. The three duplicated sequence-operator prefix matchers in
`parser/primary/container/meta_ops.rs` share `strip_sequence_op`.

Closes #14912.
