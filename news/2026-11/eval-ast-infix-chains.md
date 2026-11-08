# `EVAL($ast)` keeps infix chains and set-relation symbols

Three infix shapes lowered differently from what the parser builds for the same source:

- An imported list-associative infix (`use M; 1 lsx 2 lsx 3`, `is assoc<list>`) is an `ApplyListInfix`; the lowering folded it into nested pairwise calls (`LS(LS(1,2),3)`). A chain of three or more operands of a word the tree does not declare itself now calls the routine once with every operand, as the parser does.
- A declared comparison word (`multi infix:<eq>`) in a chain (`a eq b eq c`) is a left-nested `ApplyInfix` in rakudo's tree; it lowered to nested calls, comparing the first result with the next operand. It now lowers to the parser's `ChainedCompare`, which asks the declared candidates pair by pair.
- `∈ ∋ ⊆ ⊇ ⊂ ⊃` lowered to an undeclared infix instead of the set operator the parser reads them as, so a shaped array operand was not flattened.

Five more test files pass as `EVAL(.AST)` and join the ratchet. Test: `t/lang/eval-ast-infix-chains.t`.
