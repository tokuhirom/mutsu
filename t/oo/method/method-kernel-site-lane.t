use Test;

# #18267: a call site whose cached plan carries a method kernel runs the
# kernel on the operands where they lie on the stack. A call the lane cannot
# serve (an argument of another class, a value the kernel bails on, a
# different receiver class at the same site) must still answer exactly what
# the ordinary call answers, and the site must keep working afterwards.

plan 9;

class P {
    has $.x;
    has $.y;
    method dist(P $o) { (($!x - $o.x) ** 2 + ($!y - $o.y) ** 2).sqrt }
}
class Q is P { }
class R {
    has $.x;
    has $.y;
    method dist(P $o) { 'R:' ~ ($!x + $o.x) }
}

my $a = P.new(x => 0, y => 0);
my $sum = 0e0;
for ^500 {
    my $b = P.new(x => $_ % 4, y => 0);
    $sum += $a.dist($b);
}
is $sum, 750e0, 'a hot kernel-planned site';

# The same site, an argument of a subclass: the plan's argument key differs.
my @args = (P.new(x => 3, y => 4)) xx 50;
@args.push: Q.new(x => 6, y => 8);
my @got = @args.map({ $a.dist($_) });
is @got[*-1], 10e0, 'a subclass argument at a warm site';
is @got[0], 5e0, 'the plain argument before it';

# An attribute the kernel cannot read as a plain number bails to the frame.
my $c = P.new(x => 1/2, y => 0);
is $c.dist(P.new(x => 0, y => 0)), 0.5e0, 'a Rat attribute';
is $a.dist(P.new(x => 10**20, y => 0)), 1e20, 'a big Int accessor';

# Another receiver class at the same call site.
my @recv = |(P.new(x => 1, y => 0) xx 30), R.new(x => 1, y => 0);
my @out = @recv.map({ .dist(P.new(x => 2, y => 0)) });
is @out[0], 1e0, 'the planned receiver';
is @out[*-1], 'R:3', 'a different receiver class at the same site';

# The site keeps serving after every decline above.
my $t = 0e0;
$t += $a.dist(P.new(x => 3, y => 4)) for ^100;
is $t, 500e0, 'the site after its declines';

# A die right after a kernel call is the caller's own exception.
try { my $d = $a.dist(P.new(x => 1, y => 0)); die 'boom' }
ok $!.message eq 'boom', 'an exception after a kernel call';
