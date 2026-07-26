# RakuAST model method table

RakuAST registered type objects and node values now expose `.^method_table`. The table is keyed by
method name and contains ordinary `Method` introspection objects for the constructors and accessors
implemented by mutsu's model layer.

The implementation derives the table from the same central metadata as `.^methods(:local)`, keeping
the two introspection surfaces aligned without copying Rakudo's compiler-private `IMPL-*` API.
Regression coverage includes literal, multi-field, named-constructor, read-only, empty-surface, and
node-value cases in `t/rakuast-type-method-table.t`.
