Role composition preserves lexical type bindings in their own namespace
alongside the role body's scalar variables. Those bindings remain type objects,
and temporary aliases are restored after declaration and composition.

Multi-sub and role-method signatures resolve nominal types in their declaring
package when registered. A value mixed with a role declared inside a
parameterized role can therefore bind to that local role's parameter. A
same-named scalar remains writable, and an outer role or another declaration
with the same name retains its own type identity.
