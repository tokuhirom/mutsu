# A missing method on an `Int` subclass instance names the subclass

`class I is Int { }; I.new(42).gcd(4)` raised `X::Method::NotFound` for
invocant type `Int`, the payload the instance delegates to. The not-found error
is now raised for the receiver, so `.typename` and `.message` name `I`, as in
Rakudo (#14318).
