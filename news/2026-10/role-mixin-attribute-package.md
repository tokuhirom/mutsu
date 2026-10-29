# A role attribute of a run-time mixin is packaged by the mixin's class

`(42 but E).^attributes` listed an attribute the role `E` contributes with
`.package` set to the role (`E`). Rakudo names the mixin's anonymous class
(`Int+{E}`; `Fruit+{Tomato}` for `$fruit but Tomato`), and Data::Dump::Tree
reads the `+{Role}` tail of that name to print each attribute's origin. The
attribute now answers the mixin's type name from `.package`, while its owner
(used to read and write the value) stays the role. Data::Dump::Tree
`t/06_role_attributes` passes 11 of 11 (#15413, first two items).
