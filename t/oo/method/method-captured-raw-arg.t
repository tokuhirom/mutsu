use Test;

# A closure passing a variable it captured to an `is raw` / sigilless method
# parameter: the parameter aliases the captured variable's container, on the
# first call and on every replay of the call plan.

plan 7;

class C {
    method set(Mu $i is raw, Mu $v is raw) { $i = $v; 1 }
    method sigil(\i, $v) { i = $v; 1 }
    method get(Mu $i is raw) { $i }
    method mk(Mu $instance is raw) { -> $v { self.set($instance, $v) } }
    method mk-sigil(Mu $instance is raw) { -> $v { self.sigil($instance, $v) } }
    method reader(Mu $instance is raw) { -> { self.get($instance) } }
}
my $c = C.new;

my $x = 0;
my $f = $c.mk($x);
$f($_) for ^5;
is $x, 4, 'a raw parameter writes through to the captured variable, on every call';

my $y = 0;
my $g = $c.mk-sigil($y);
$g($_ * 10) for ^4;
is $y, 30, 'a sigilless parameter writes through to the captured variable';

my $z = 'a';
my $r = $c.reader($z);
my @seen;
for <a b c> { $z = $_; @seen.push: $r() }
is @seen.join, 'abc', 'a raw parameter reads the captured variable\'s current value';

# A closure over a variable of its own block captures it too.
my @out;
for ^3 -> $i {
    my $w = $i;
    my $h = -> $v { $c.set($w, $v) };
    $h($i * 2);
    @out.push: $w;
}
is @out.join(','), '0,2,4', 'a fresh captured variable per iteration is aliased';

# A computed argument is no alias of the variable it was computed from.
my $base = 5;
my $k = -> { $c.get($base + 1) };
is $k(), 6, 'a computed argument binds its value';
dies-ok { -> { $c.set($base + 1, 9) }() }, 'a computed argument to a raw parameter is read-only';
is $base, 5, '... and does not write the variable it was computed from';
