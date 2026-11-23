# `CALLER::OUTER::` and `CALLER::UNIT::` read the caller's lexical scopes

`$CALLER::OUTER::x` names the `$x` of the scope around the *calling*
frame, and `$CALLER::UNIT::x` the `$x` of the caller's compilation unit.
Both now follow the caller frame's static link (ADR-12529 amendment A1, #18787):

```raku
{
    sub f1($f) { my $x is dynamic = 100; $f() }
    my $x = 102;
    { my $x = 104; say f1({ $CALLER::OUTER::x }) }   # 102, as on rakudo (was 104)
}
my $f = EVAL 'my $u is dynamic = 113; -> $fn { my $u is dynamic = 114; $fn() }';
say $f({ $CALLER::UNIT::u });                        # 113 (was Nil)
```

A program whose source mentions such a chain sets a latch when it is
parsed. Under the latch, a sub declared below its unit's top level captures
its declaring scope's whole static view: every lexical that scope binds, as
shared cells taken from the right slot. A by-name call from the declaring
code runs that activation's code object. A reflective closure created in an
`EVAL`'d unit's mainline now records the unit, so `UNIT::` of a frame
running it is the `EVAL`'s mainline rather than the program's. Apart from
that `EVAL` case, a program without such a chain compiles to the same
bytecode as before.

`S02-names/pseudo-6d.t`, `6.c/S02-names/pseudo-6c.t` and
`S02-names/pseudo-6e.t` now pass completely and are whitelisted.
