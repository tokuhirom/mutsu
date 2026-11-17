# `nqp::eqaddr` compares containers, as `=:=` does

Rakudo's `infix:<=:=>` is `nqp::eqaddr` of its raw operands, but mutsu gave
the two separate implementations: `nqp::eqaddr` decontainerized its operands
and FETCHed a Proxy, so `my $x = @a; nqp::eqaddr($x, @a)` answered 1 where
rakudo answers 0 (`$x` is a Scalar, not `@a`). `=:=`, for its part, fell back
to `===`'s `.WHICH`-aware comparison, so two objects of a class with a
constant user `WHICH` were `=:=`.

They are now one primitive (#17620). The compiler's `=:=` lowering, which
keeps what it knows about each operand's binding, moved to
`compiler/container_identity.rs`, and `nqp::eqaddr(a, b)` compiles to it plus
an int coercion. Its value-level fallback is the object identity
`nqp::eqaddr` always used (`values_same_object`). The typed tier (TRIR), whose
operands are always bare values, keeps an `nqp::eqaddr` only when no operand
can be a container (a `$` variable, an element or a sigilless local).
