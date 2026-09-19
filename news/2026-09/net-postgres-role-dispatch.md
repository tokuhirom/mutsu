# Net::Postgres role dispatch

mutsu now supports the package-scoped role and type-object dispatch patterns
used by Net::Postgres 0.0.4. Multi candidates resolve private nominal roles in
their declaration package, nested-package roles are recognized during binding,
and role methods may use enum members as literal value parameters.
