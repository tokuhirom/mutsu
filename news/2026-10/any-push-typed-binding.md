# `Any.push` vivifies any type object a binding holds, under its type

`Any`'s growing methods (`push`, `append`, `unshift`, `prepend`) used to
autovivify only a binding that held `Nil`, `Any`, `Mu` or (for a named binding)
`Array`. A binding holding any other type object fell through to
`X::Method::NotFound`: `my $x = Int; $x.push(1)` died, and `my Int $x;
$x.push(3)` reported a missing method where Rakudo reports a type check.

Rakudo's candidate is `multi method push(Any:U \SELF: |values)`, so every
undefined invocant matches it. The binding then vivifies to an `Array` through
an ordinary assignment. A binding holding any non-role type object now
vivifies the same way, and that assignment obeys the binding's declared type,
read from the slot its code declares (ADR-13757). `my Int $x; $x.push(3)` dies
with `X::TypeCheck::Assignment` ("expected Int but got Array"), while
`my $x = Int` and `my Cool $c` take the Array (#13823).
