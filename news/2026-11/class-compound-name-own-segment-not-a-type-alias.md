# `class Y::Errno` no longer shadows an imported `Errno` inside itself

Declaring `class Y::Errno` registered the short name `Errno` as a type alias of
`Y::Errno` under `Y`, and a method of that class (running as `Y::Errno`, which
is inside `Y`) therefore resolved the bare `Errno` of `has Errno $.errno` to the
class itself instead of an imported enum of that name, failing the construction
type check. A class declared with a compound name outside its parent prefix is
not declared inside that parent, so its own last segment is no binding for the
code running in it (#16035).
