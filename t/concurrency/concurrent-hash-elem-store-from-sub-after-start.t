use Test;

# An element store into an outer hash/array from a sub or method must stay
# visible once a `start` block has been spawned (and not yet awaited).
plan 9;

my %G; sub g($x) { %G{$x} = 1 }
g('a'); my $p = start { 1 }; g('b');
is %G.keys.sort.join(','), 'a,b', 'hash store from a sub after start';

my %I; sub i($x) { %I{$x} = 1 }
i('a'); my $q = start { sleep 0.1 }; i('b'); i('c');
is %I.keys.sort.join(','), 'a,b,c', 'hash stores while a thread is sleeping';

my %R; sub r($x) { %R{$x} = 1; %R.keys.sort.join(',') }
my $s = start { 1 };
is r('b'), 'b', 'the sub re-reads its own store';
is r('z'), 'b,z', 'a second store accumulates';

my @E; sub e($x) { @E[$x] = 1 }
e(0); my $t = start { 1 }; e(1);
is @E.elems, 2, 'array store from a sub after start';

class K {
    our %H;
    method put($k, $v) { %H{$k} = $v }
    method go { start { 1 }; }
}
my $k = K.new;
$k.put('a', 1); my $u = $k.go; $k.put('b', 2);
is K::<%H>.keys.sort.join(','), 'a,b', 'our hash store from a method after start';

class L {
    our @A;
    method put($i, $v) { @A[$i] = $v }
    method go { start { 1 }; }
}
my $l = L.new;
$l.put(0, 1); my $w = $l.go; $l.put(1, 2);
is L::<@A>.elems, 2, 'our array store from a method after start';

await $p, $q, $s, $t, $u, $w;
is %G.keys.sort.join(','), 'a,b', 'still visible after await';
is %I.keys.sort.join(','), 'a,b,c', 'still visible after await (2)';
