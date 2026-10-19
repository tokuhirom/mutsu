use Test;

# A class is `our`-scoped: declared in a block, it is the package's and
# outlives the block. A block that also `use`s or `import`s a module rolled
# its classes back with the import scope (ADR-8751's class-table rollback),
# so the class was gone once the block exited (roast S11-modules/export.t's
# `class C1 does Bar[42]` after `import Foo`). Only a `my class` is lexical.

plan 6;

{
    my module Foo { role Bar is export { method bar { "bar" } } }
    import Foo;
    class UbdcC1 does Bar { }
}
is UbdcC1.new.bar, 'bar', 'a class declared after an import outlives the block';

{ use Test; class UbdcK1 { method m { 'K1' } } }
is UbdcK1.m, 'K1', 'a class declared in a block that uses a module outlives it';

{ use Test; role UbdcR1 { method r { 'R1' } } }
is (class :: does UbdcR1 {}).r, 'R1', 'so does a role';

{ use Test; grammar UbdcG1 { token TOP { a } } }
is UbdcG1.parse('a').Str, 'a', 'and a grammar';

sub ubdc-mk { use Test; class UbdcK2 { method m { 'K2' } }; UbdcK2 }
ubdc-mk();
is UbdcK2.m, 'K2', 'a class declared in a routine body that uses a module outlives the call';

{ use Test; my class UbdcL1 { } }
nok ::('UbdcL1').defined, 'a `my class` in such a block stays lexical to it';
