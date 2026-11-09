use Test;

# `Mu.CREATE` on a DEFINED invocant allocates a fresh, uninitialised object of
# the same class (rakudo: `nqp::create(self)`), as a `clone` override needs
# (`self.CREATE!SET-SELF(...)`).
plan 7;

class H {
    has @.a;
    has $.n;
    method !SET(@v, $n) { @!a = @v; $!n = $n; self }
    method copy { self.CREATE!SET(@!a.clone, $!n) }
}
class M is H { }

my $m = M.new(a => [1, 2], n => 7);
my $c = $m.copy;
isa-ok $c, M, "CREATE on an instance keeps the (sub)class";
ok $c !=== $m, "it is a new object";
is $c.a, [1, 2], "state set by a private method";
is $c.n, 7, "scalar attribute";
$c.a.push(3);
is $m.a, [1, 2], "the original is unchanged";

my $fresh = $m.CREATE;
isa-ok $fresh, M, "plain .CREATE on an instance";
nok $fresh.n.defined, "no attribute is initialised";
