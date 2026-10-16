# Attribute, IterationBuffer and Routine methods are method-table rows

The `Attribute` metaobject's methods (`name`, `type`, `rw`, `get_value`, `set_value`, `build`, ...), `IterationBuffer`'s
(`push`, `AT-POS`, `BIND-POS`, `append`, `List`, ...) and `Routine`'s `is_dispatcher`, `dispatcher`, `multi` and
`package` are now rows of the classes Rakudo declares them on (ADR-11276 §9.121), in place of name-matching arms in the
interpreter's slow path. `Routine`'s methods had two copies, one for a `Sub` and one for a `&name` handle; each is now
one implementation. `Code.set_name` is one row for a sub and a regex. `.^can` sees all of them, and the runtime slow
path lost 31 quoted-name arms.
