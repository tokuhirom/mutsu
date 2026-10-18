# A module's `my T $x .= new` stays the module's own lexical

The parser expands `my T $x .= new;` and `my ($a, $b) = ...;` into a
synthetic block: the declarations plus the statement that initializes them.
The passes that collect a module's file-scope declarations at load time only
looked at top-level `VarDecl`s, so they missed these. The variable then
stayed in the importer's env instead of moving into the module's own
unit-lexical store. When a second module declared the same name, its routines
read the first module's value: in BDD::Behave, `Matcher::Custom`'s
`$REGISTRY.build(...)` ran on the `SpecRegistry` object.

`ast::scope_statements` opens up synthetic blocks (a parser desugaring, not a
scope) for the unit-lexical, `our`-variable and own-dynamic collectors.
BDD::Behave `t/expectations/custom-matchers.rakutest` now passes 67/67.
