use Test;

# A method with a parameter that may alias the caller's container (sigilless,
# `is raw`, `is rw`) is planned like any plain method (#18267 stage 3): each
# replay re-checks where the arguments came from and promotes a caller
# variable to its shared cell. Every case runs in a loop so the plan is
# installed and then replayed.

plan 12;

class C {
    method set-raw(Mu $target is raw, $v) { $target = $v }
    method set-sigilless(\target, $v) { target = $v }
    method set-rw($target is rw, $v) { $target = $v }
    method read-raw(Mu $x is raw) { $x }
    method read-sigilless(\x) { x }
    method same(Mu $a is raw, Mu $b is raw) { $a =:= $b }
}

my $o = C.new;

my $r = 0;
$o.set-raw($r, $_) for 1..5;
is $r, 5, 'an is raw parameter writes the caller variable on every call';

my $s = 0;
$o.set-sigilless($s, $_) for 1..5;
is $s, 5, 'a sigilless parameter writes the caller variable on every call';

my $w = 0;
$o.set-rw($w, $_) for 1..5;
is $w, 5, 'an is rw parameter writes the caller variable on every call';

my @vals = (^5).map: { $o.read-raw($_) };
is-deeply @vals, [0, 1, 2, 3, 4], 'an is raw parameter given a value';

@vals = (^5).map: { $o.read-sigilless($_ * 2) };
is-deeply @vals, [0, 2, 4, 6, 8], 'a sigilless parameter given a value';

my $dies = 0;
for ^5 { try { $o.set-raw(42, $_); CATCH { default { $dies++ } } } }
is $dies, 5, 'an is raw parameter given a literal cannot be assigned';

$dies = 0;
for ^5 { try { $o.set-rw(42, $_); CATCH { default { $dies++ } } } }
is $dies, 5, 'an is rw parameter refuses a literal';

# Alternating a variable and a value through the same call site: the replay
# must re-check the argument each time.
my $alt = 0;
my @alt-dies;
for ^6 -> $i {
    if $i %% 2 { $o.set-raw($alt, $i) }
    else { @alt-dies.push: so (try { $o.set-raw(7, $i); True }) }
}
is $alt, 4, 'a variable argument after value arguments is still aliased';
is-deeply @alt-dies, [False, False, False], 'a value argument after variable arguments is still refused';

my $x = 1;
my @same = (^3).map: { $o.same($x, $x) };
is-deeply @same, [True, True, True], 'two raw parameters given one variable share its container';

my %h = a => 1;
$o.set-raw(%h<a>, $_) for 1..3;
is %h<a>, 3, 'an is raw parameter given a hash element writes it';

my @a = 0, 0;
$o.set-sigilless(@a[1], $_) for 1..3;
is-deeply @a, [0, 3], 'a sigilless parameter given an array element writes it';
