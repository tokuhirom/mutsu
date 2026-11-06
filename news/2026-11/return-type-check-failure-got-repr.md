# A failed return-type check shows the value's `.raku` and class

`X::TypeCheck::Return` now reports a List, Array, Hash or object by its `.raku`
and names an instance by its class (`got F (F.new)`, not `got Any (F<21>)`),
as rakudo and mutsu's own binding errors do (#16842).
