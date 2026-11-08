# `EVAL($ast)` keeps infix chains; set operators called by name read shaped arrays

Three infix shapes lowered differently from what the parser builds for the same source:

- An imported list-associative infix (`use M; 1 lsx 2 lsx 3`, `is assoc<list>`) is an `ApplyListInfix`; the lowering folded it into nested pairwise calls (`LS(LS(1,2),3)`). A chain of three or more operands of a word the tree does not declare itself now calls the routine once with every operand, as the parser does.
- A declared comparison word (`multi infix:<eq>`) in a chain (`a eq b eq c`) is a left-nested `ApplyInfix` in rakudo's tree; it lowered to nested calls, comparing the first result with the next operand. It now lowers to the parser's `ChainedCompare`, which asks the declared candidates pair by pair.
- `3 ∈ @c` over a multi-dimensional shaped array (or a user `Iterable` instance) was False through `EVAL($ast)`, and the routine forms `infix:<∈>(3, @c)` / `&infix:<(elem)>(3, @c)` were False even in a plain program: the operator form walks the operands through their leaves and elements, the routine form did not. Every call of a set operator by name (`infix:<(elem)>`, `∈`, `⊆`, ...) now walks them the same way (`is_set_operator`, shared by the routine call and the VM's infix fallback).

Five more test files pass as `EVAL(.AST)` and join the ratchet. Test: `t/lang/eval-ast-infix-chains.t`.
