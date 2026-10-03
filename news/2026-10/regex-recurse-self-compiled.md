# `<~~>` recursion runs on the compiled regex engine

A regex that recurses into itself with `<~~>` (balanced parentheses:
`rx/ '(' [ <-[()]>+ | <~~> ]* ')' /`) used to be declined by the compiled regex engine as a whole
pattern and matched by the tree walk. It now compiles (ADR-0135 Slice E, twelfth part): each `<~~>`
matches the enclosing regex through its compiled program. The results are unchanged and agree with
rakudo.
