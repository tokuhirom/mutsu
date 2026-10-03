# A user-declared enum member shadows the `e`, `pi` and `tau` constants

`enum Index <r w x e d>` now makes the bareword `e` the enum member instead of Euler's number,
as in Rakudo. The keyword-literal parser consulted the built-in constants before the declared enum
values. Found through the `P5-X` distribution, whose `-e` file test indexed an enum with `e`;
its `t/01-basic.rakutest` now passes 33/33 under mutsu.
