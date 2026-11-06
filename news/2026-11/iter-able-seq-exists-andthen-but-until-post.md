# Iter::Able: Seq `:exists`, `andthen` over a mixin value, `nqp::until` post step

Three gaps found running the Iter::Able suite (#16939) are fixed:

- `$seq[$i]:exists` read a `Seq` as having no slots; it now answers from the Seq's cache like `Seq.EXISTS-POS`.
- `X andthen Y but role {...}` treated the mixin result as a callable and invoked it with the topic, yielding `Nil`. Only a mixin over a code object is callable now.
- `nqp::while`/`nqp::until` (and the `repeat_` forms) with a third `post` operand evaluated it as a plain call argument, so `Min-Max`'s helper ran the body on an empty iterator. The post step now runs after each body.

Iter::Able t/16, t/24 and t/25 now match rakudo. Still different: t/18/t/19 (`&[=:=]` called with variables), t/29 (an immutable list element seen through a routine parameter), t/17/31/32 (#16934).
