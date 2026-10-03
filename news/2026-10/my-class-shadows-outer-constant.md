# A `my class` shadows a same-named outer `constant`

```raku
constant RIS = Int;
{ my class RIS { method hi { "class" } }; say RIS.hi }   # class
say RIS.^name;                                           # Int
```

Inside the inner block, the bareword `RIS` used to keep naming the outer
constant: the compiler still had it in scope (and inlined `Int`), and a closure's
run-time bareword lookup found the constant's term key before the type. A
lexical type declaration (`my class` / `my role` / `my grammar`) now drops a
visible same-named constant for the rest of its block. A bareword of that name
reads the type's lexical binding instead, in the block, in nested closures and
in the type's own methods. The constant is visible again after the block, and a
nested `constant` of the same name shadows the type in turn (#11517).
