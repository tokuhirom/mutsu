RakuAST now preserves attribute `handles` terms across conversion and EVAL,
including rename pairs, dynamic lists and regex delegation. Bare identifiers
in a handles list remain delegated method names, while identifiers in a
slipped expression keep their expression behavior.
