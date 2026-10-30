use Test;

# A `for` pointy parameter marked `is raw` aliases the element's container, so
# assigning to it writes through (a Proxy element runs its STORE).

plan 4;

sub mk() is raw { my $s = 0; Proxy.new: FETCH => -> | { $s }, STORE => -> | { $s = 9 } }

my $ok = False;
for (mk(),) -> Mu $b is raw { $b = 5; $ok = True }
ok $ok, 'assignment to an is-raw loop parameter bound to a Proxy lives';

sub t(+@x is raw) { for @x -> Mu $b is raw { $b = 5 }; 'done' }
is t(mk()), 'done', 'also through a slurpy raw parameter';

my @seen;
sub rec($key) is raw {
    my $s;
    Proxy.new: FETCH => -> | { $s }, STORE => -> $, $v { @seen.push("$key=$v"); $s = $v }
}
my $i = 0;
for (1, 2).map({ rec("k$_") }) -> Mu $b is raw { $b = $i++ }
is @seen.join(' '), 'k1=0 k2=1', 'each element\'s STORE ran once';

my $p := rec("z");
$p = 3;
is @seen.tail, 'z=3', 'a bound Proxy stores as before';
