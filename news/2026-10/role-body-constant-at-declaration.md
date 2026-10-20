# A role body's `my constant` is initialized when the role is declared

Rakudo initializes a `constant` when the code around it is compiled, and a
role body is no exception. A `sub` declared in the role body and exported
(`sub dimension(\u) is export(:dimension) { %Dimension{u} }`) therefore reads
the constant in an importer that never composes the role.

mutsu deferred every non-declaration statement of a role body to composition,
the constant included. An imported `dimension('pt')` answered `Any`, and
`plain()` reading `%D.elems` answered `0`. That is why CSS::Units could not
resolve its units (`Unknown units: pt` in CSS::Properties' measuring code).

The eager declaration pass that already registered a role body's exported
variables, types and subsets now runs its `constant`s as well. In a
parameterized role, a constant that mentions a role parameter still waits for
composition, where the parameter has a value.
