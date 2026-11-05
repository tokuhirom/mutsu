# A `require`d module's last statement no longer replaces the importer's `$_`

`for <a> { require Foo; say $_.raku }` printed the loaded type object where rakudo prints `"a"` (#15959):
the mainline of a module loaded at run time runs in the importer's env, and the compiler delivered a unit's tail
value through `$_` (`SetTopic`, the mainline/EVAL result convention), so the module's last statement became the
importer's topic. The value depends on the module: a class declaration left its type object, a trailing `1;` left 1,
a role left `Any`. The same bug had been fixed for `gather` bodies (`unit_tail_discards`) and `EVAL` units
(`unit_tail_on_stack`); the two module-mainline compile entry points now set `unit_tail_discards` too, since nobody reads
a module mainline's value. This is the `require ::($_)` half of Red's `schema(<Post Person>)`: Red's
`t/052-schema.rakutest` passes 20/20 on mutsu now, as on rakudo.
Pinned by `t/modules/import-export/require-class-module-keeps-topic.t`.
