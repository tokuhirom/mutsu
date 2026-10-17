RakuAST now preserves shaped array parameters. Their synthesized
`array-shape` predicate remains accessible to introspection while the
constructor-form gist matches Rakudo by omitting it. Lowering restores the
dimension expressions to the parameter binder, preserving one-dimensional,
multidimensional, and lexical shape checks.
