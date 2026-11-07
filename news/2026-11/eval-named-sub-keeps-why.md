# A named sub returned by `EVAL` keeps its `.WHY`

`my $c = EVAL "#| lead\nsub bar { }; &bar"; $c.WHY` answered `Nil`: the documentation of a named
routine lives in a name-keyed table that is put back when the `EVAL` unit ends. The docs of the
routine the unit answers are now filed under the routine object before the table is restored, so
`.WHY` gives the `Pod::Block::Declarator` as in rakudo. Closes #17050.
