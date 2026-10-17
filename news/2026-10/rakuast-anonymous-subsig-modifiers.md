# RakuAST: anonymous destructuring parameter modifiers

Anonymous positional destructuring parameters now preserve their outer
default, `where` constraint, and `is` traits in the RakuAST tree alongside the
nested `Signature`. Lowering feeds the constraint and trait metadata through
the existing parameter binder.

The focused regression checks Rakudo's field order and omission rules, EVAL of
where-constrained and trait-bearing parameters, and where-constraint
enforcement.
