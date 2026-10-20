Role composition now preserves lexical type bindings in their own namespace
alongside the role body's scalar variables. Those bindings remain type objects
instead of being boxed as mutable scalar statics.

Signature checks and type aliases consult that namespace before a same-named
scalar or outer type. A value mixed with a role declared inside a parameterized
role can therefore bind to that local role's parameter, while another
declaration with the same name remains a distinct type.
