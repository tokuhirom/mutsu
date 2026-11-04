# A class's own method on its type object beats the vivifying row

`Cls.ASSIGN-POS(0, $v)` with a `multi method ASSIGN-POS` in `Cls` died with
"Cannot modify an immutable 'Cls' type object". The built-in `Any` row for the
methods that vivify an undefined receiver (`push`, `ASSIGN-POS`, ...) was tried
first on every type object, and a class's MRO always reaches `Any`. The row now
steps aside when the type object's class defines the method itself.

Found with Functional::LinkedList, whose `$f1[1] = 2` recurses through
`$!next.ASSIGN-POS: :internal, $index - 1, $value` on a type object; its test
file now matches rakudo (60 of 60, was 7).
