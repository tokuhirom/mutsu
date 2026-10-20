# Package `our` bindings keep their own containers

A package routine now reads its own `our` scalar on its first TRIR call,
including when its caller declares the same variable name. Previously the
first call could read the caller's value and subsequent bytecode calls read
the package's value. The RakuAST frontend exposed this through its different
call metadata; the fix resolves the binding in the execution engine.

Plain assignments and `is rw` arguments also use the package's canonical
cell. A resolved argument container takes precedence over its source spelling
during parameter binding, so a callee neither substitutes a caller lexical
nor copies its result back into that unrelated variable. Readonly decisions
and type constraints follow the actual binding. Package scalar declarations
carry their own constraint in bytecode instead of inheriting same-named
caller metadata; the precompilation cache format is now 34.

Class-body `our` declarations retain package storage while restoring their
bare lexical aliases when the body exits. Changes through an outer shared
alias remain visible. The regression exercises both string and AST EVAL,
first and repeated calls, nested packages, classes, parameters, aliases and
typed or readonly caller variables against Rakudo 2026.07.

This is a Stage 1 S10 slice of #7564, tracked by #14111 and ADR-10723.
