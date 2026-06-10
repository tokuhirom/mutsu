use Test;

# Constructor correctness for `is Type` and shaped container attributes.
#
# 1. Shaped typed arrays (`has Int @.g[2;2]`) must not crash during
#    construction (regression from typed-`@`/`%` element type-checking, which
#    naively treated the per-dimension sub-arrays as elements) and must carry
#    the element type (`.of` == Int).
# 2. `is Type` container attributes (`has @.a is Buf`) must coerce a *provided*
#    value to the declared container type, not just leave it a plain Array/Hash.

plan 19;

# --- shaped typed array: no crash, correct shape + element type ---
class Grid { has Int @.g[2;2]; }
my $grid = Grid.new();
is $grid.g.^name, 'Array[Int]', 'shaped typed array .WHAT is Array[Int]';
is $grid.g.shape, (2, 2), 'shaped typed array keeps shape';
is $grid.g.of.^name, 'Int', 'shaped typed array .of is Int';

class Row { has Int @.r[3]; }
my $row = Row.new(r => [1, 2, 3]);
is $row.r, [1, 2, 3], 'shaped typed array provided values';
is $row.r.shape, (3,), 'shaped typed array provided keeps shape';
is $row.r.of.^name, 'Int', 'shaped typed array provided .of is Int';

# shaped untyped still works
class Plain { has @.p[2;2]; }
my $plain = Plain.new();
is $plain.p.shape, (2, 2), 'shaped untyped array keeps shape';

# bad element in shaped typed array dies (leaf-level check)
dies-ok { Grid.new(g => [[1, 'x'], [3, 4]]) }, 'bad leaf element in shaped array dies';

# --- is Type container attributes: provided value coercion ---
class C {
    has @.a is Buf;
    has %.h is BagHash;
}
# uninitialized
my $c = C.new();
ok $c.a ~~ Buf, 'is Buf uninit is a Buf';
ok $c.h ~~ BagHash, 'is BagHash uninit is a BagHash';

# provided values coerce to the declared container type
my $c2 = C.new(a => [1, 2, 3], h => <x y x>);
ok $c2.a ~~ Buf, 'is Buf provided value coerces to Buf';
is $c2.a.elems, 3, 'is Buf provided has the right elems';
is $c2.a[0], 1, 'is Buf provided keeps values';
ok $c2.h ~~ BagHash, 'is BagHash provided value coerces to BagHash';
is $c2.h<x>, 2, 'is BagHash provided counts duplicates';

# an already-correct value is kept as-is
my $buf = Buf.new(9, 8);
my $c3 = C.new(a => $buf);
ok $c3.a ~~ Buf, 'already-Buf provided value stays a Buf';
is $c3.a[0], 9, 'already-Buf value preserved';

# inheritance: is Type attribute on a parent
class Base { has @.b is Buf; }
class Derived is Base { }
my $d = Derived.new(b => [7, 7]);
ok $d.b ~~ Buf, 'inherited is Buf attribute coerces provided value';
is $d.b[1], 7, 'inherited is Buf keeps values';

# done
