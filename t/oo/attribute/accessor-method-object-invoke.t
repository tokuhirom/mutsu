use Test;

# Invoking the Method object of an auto-generated attribute accessor runs the
# accessor, whichever MOP route handed the object out (#10220).

plan 16;

class D {
    has $.x;
    has $.y is rw;
    has $!z = 7;
    method z() is rw { $!z }
}

my $d = D.new(x => 1, y => 2);

is D.^can("x")[0]($d), 1, '.^can accessor object reads the attribute';
is D.^lookup("x")($d), 1, '.^lookup accessor object reads the attribute';
is D.^find_method("x")($d), 1, '.^find_method accessor object reads the attribute';
is D.^can("y")[0]($d), 2, 'is rw accessor object reads the attribute';
is D.^methods.first(*.name eq "x")($d), 1, '.^methods accessor object reads the attribute';
is $d.can("x")[0]($d), 1, 'instance .can accessor object reads the attribute';

D.^can("y")[0]($d) = 5;
is $d.y, 5, 'assigning through an is rw accessor object writes the attribute';
D.^lookup("y")($d) = 6;
is $d.y, 6, 'assigning through a .^lookup is rw accessor object';
throws-like { D.^lookup("x")($d) = 9 }, X::Assignment::RO,
    'assigning through a read-only accessor object dies';
is $d.x, 1, 'the read-only attribute is unchanged';

D.^lookup("z")($d) = 9;
is $d.z, 9, 'assigning through an is rw method object writes the attribute';
my $m = D.^lookup("z");
$m($d) = 10;
is $d.z, 10, 'assigning through a stored is rw method object';

sub get-d { $d }
get-d().z = 11;
is $d.z, 11, 'is rw method lvalue on a call result writes the shared instance';

class C { my $.count = 3; has $.v is rw = 1 }
is C.^lookup("count")(C), 3, 'class-level accessor object reads the attribute';

my $w = C.^find_method("v");
$w.wrap(-> $s { callsame() + 100 });
my $c = C.new;
is $w($c), 101, 'invoking a wrapped accessor object runs the wrapper';
is $c.v, 101, 'the wrap still applies to ordinary dispatch';
