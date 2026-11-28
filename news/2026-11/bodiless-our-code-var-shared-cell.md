# A bodiless `our &t` shares its cell with `&S::t`

`our &t = sub { ... }` already shared one cell with the package-qualified `&S::t`;
a bodiless `our &t;` kept two independent stores, so `t()` after a class-body
`&t = ...` failed with "Unknown function", and a method or sub body assigning
`&t` raised "Cannot modify an immutable value". The bodiless declaration now
uses the same shared-cell opcode (keeping an existing package value on
redeclaration), and the class body's `our &t` names are inherited into method
and sub bodies, so the assignment writes the package cell.
