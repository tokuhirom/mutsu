# RakuAST resolves a declared name, and `.^name` is a metamethod

Two read gaps, both measured against rakudo 2026.07 and byte-for-byte identical
there, both lowering back through `EVAL`.

## A bareword the same unit declared

`class C { }; C.new` was a `.AST` boundary. raku resolves such a name at parse
time — a declared *type* renders as `RakuAST::Type::Simple`, exactly like a
builtin type, and a declared *constant* renders as `RakuAST::Term::Name` — but
mutsu's parser leaves both as `Expr::BareWord`, and only builtin type names
(`is_known_type_constraint`) converted. So *any* program that declared a class,
role, enum, subset or constant and then used it could not be rendered at all.

That is what forced every declaration slice so far — `t/rakuast-eval-class.t`,
`t/rakuast-class-traits-multi-constant.t` — to end its program with the
declaration and inspect the `EVAL`'d value from the *outside*. Those programs can
now use the name where it was declared.

The converter collects the unit's declared names before converting (a walk over
the statement list, descending into blocks, packages and routine bodies) and
keeps them in a scoped thread-local for the duration, restoring the previous set
on the way out so a nested conversion cannot leak into the outer one. A bareword
that names nothing the unit declared is still the boundary it was.

## `.^name`

`.^name` rendered as an ordinary `Call::Method` carrying a `dispatch => ".^"`
field, alongside `.?` / `.+` / `.*`. raku gives it a class of its own,
`RakuAST::Call::MetaMethod`, whose `name` is a plain string rather than a `Name`
node. `.?` / `.+` / `.*` really are dispatch modifiers and are unchanged.

mutsu keeps `^` in the same `modifier` slot as the dispatch modifiers
internally, so this is purely a rendering split; the lowerer puts it back.

## Coverage

`t/rakuast-declared-name.t` (14 assertions) pins a declared class and role
rendering as `Type::Simple`, a declared constant as `Term::Name` (and *not* as a
type), the metamethod shape and the absence of a `dispatch` field on it, `.?`
still carrying one, and six `EVAL` round trips that use the declared name inside
the lowered program — a constant in an expression, a class called and
constructed by name, and a class composing a role. It is a dual-oracle test: it
passes verbatim under both mutsu and rakudo 2026.07.
