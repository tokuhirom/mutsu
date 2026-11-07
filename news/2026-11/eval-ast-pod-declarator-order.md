# `EVAL($ast)` keeps Pod and declarator blocks in tree order

A tree evaluated with `EVAL($ast)` listed its `#|` declarator blocks after its Pod blocks in
`$=pod`. The declarations of the tree are now placed by the length of the Pod text before them (the
tree has no source offsets), so the order is the tree's, as for the source itself (#17070 did the
source side). Part of #16356.
