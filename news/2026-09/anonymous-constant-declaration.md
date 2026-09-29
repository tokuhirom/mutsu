# The anonymous `constant = EXPR` declaration parses

`constant = "anon";` used to be rejected with `X::Syntax::Missing` (missing initializer). It is now
accepted as an anonymous constant: the initializer is evaluated and bound to a unique hidden name, so
several anonymous constants in one scope do not collide. Fixes #9872.
