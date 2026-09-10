use v6;
use Test;

# `handles <AT-POS ASSIGN-POS>` on an `@`/`%` attribute forwards through a
# delegation method. The non-mut dispatch path passed the attribute's container
# to the target method *by value*, so a mutating target (`ASSIGN-POS`, `push`,
# `ASSIGN-KEY`, ...) updated a copy and the write vanished. It only ever looked
# to work on a punned role, whose attributes lived in mixin markers.

plan 8;

class Pos does Positional {
    has @!c handles <AT-POS ASSIGN-POS BIND-POS>;
    method raw() { @!c }
}

my $p = Pos.new;
$p[0] = 11;
$p[1] = 22;
is "$p[0] $p[1]", '11 22', 'subscript assign through handles persists on a class';
is $p.raw.elems, 2, 'the attribute itself was mutated, not a copy';

$p[0] := 99;
is $p[0], 99, 'subscript bind through handles persists';

class Assoc does Associative {
    has %!h handles <AT-KEY ASSIGN-KEY>;
    method raw() { %!h }
}

my $a = Assoc.new;
$a<x> = 1;
$a<y> = 2;
is "$a<x> $a<y>", '1 2', 'key assign through handles persists on a class';
is $a.raw.elems, 2, 'the hash attribute itself was mutated';

# Same, reached by composing a role (parameterised and not).
role PosR does Positional { has @!c handles <AT-POS ASSIGN-POS>; }
role PosP[::T] does Positional { has @!c handles <AT-POS ASSIGN-POS>; }
class ViaRole does PosR { }
class ViaParam does PosP[Int] { }

my $r = ViaRole.new;
$r[0] = 5;
is $r[0], 5, 'class composing a plain role persists the delegated assign';

my $q = ViaParam.new;
$q[0] = 6;
is $q[0], 6, 'class composing a parameterised role persists the delegated assign';

my $pun = PosP[Int].new;
$pun[0] = 7;
is $pun[0], 7, 'the parameterised pun persists the delegated assign';
