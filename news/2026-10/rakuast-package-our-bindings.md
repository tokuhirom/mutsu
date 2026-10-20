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
caller metadata. Split package bodies also carry their own `our` aliases, so
their runtime initializers cannot reuse a caller's same-named slot. The
precompilation cache format is now 37.

Class-body `our` declarations retain package storage while restoring their
bare lexical aliases when the body exits. Changes through an outer shared
alias remain visible. The regression exercises both string and AST EVAL,
first and repeated calls, nested packages, classes, parameters, aliases and
typed or readonly caller variables against Rakudo 2026.07.

Module and package declarations also preserve and execute their `is` traits
through compiled multi dispatch, including arguments, source order beside
export traits, unit bodies and hand-built AST nodes. Qualified declarations
create their ancestor namespaces; unresolved qualified terms, post-declared
modules/packages and illegal semicolon forms raise their proper exceptions.
Package/module declarations also work as expression values, retaining lexical
scope (`my $p = my package P {}`). All 83 assertions of
`roast/S10-packages/basic.t` now pass through both frontends, and the file joins
the whitelist.

The runtime and AST conversion share the generated CORE type-name catalogue.
Generated export namespaces use separate unit and qualified-package indexes.
Class/role plans carry
explicit compiler qualification instead of guessing from a package prefix.
Body chunks use that same provenance, and relative compound type identities
remain distinct from their written bindings. Generated namespaces preserve
custom EXPORT lookup, and a vivified ancestor does not replace a pending
class's HOW.
Literal stash subscripts retain their lookup semantics: a nested type is a
type object, while an absent key yields Nil. EVAL also retains the namespace
classification of visible generated export stashes.
The declaration-phase timing of trait arguments remains tracked in #14146.

This is a Stage 1 S10 slice of #7564, tracked by #14111 and ADR-10723.
