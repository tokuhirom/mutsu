use Test;

# A routine or `do` block that holds a CATCH returns a Failure its tail
# statement yields: only a `try` throws it into the handler, and a tail the
# CATCH follows is sunk, which throws it (Red's `automatic-transaction`
# returns a Failure from a block run under CATCH, t/097).

plan 6;

class E is Exception { has $.message = 'err' }

sub a(&b) { CATCH { default { flunk 'the routine CATCH ran' } }; b() }
my $e = E.new;
my $f = a({ Failure.new($e) });
isa-ok $f, Failure, 'a routine returns the tail Failure';
ok $f.exception === $e, 'with its exception';

my $d = do { CATCH { default { flunk 'the do CATCH ran' } }; Failure.new($e) };
isa-ok $d, Failure, 'so does a do block';

my $caught = 0;
my $t = try { CATCH { default { $caught++ } }; Failure.new($e) };
is $caught, 1, 'a try throws it into its CATCH';

sub s2 { fail 'boom' }
my $sunk = 0;
sub r { s2(); CATCH { default { $sunk++ } } }
r();
is $sunk, 1, 'a tail the CATCH follows is sunk and throws';

sub r2 { CATCH { default { flunk 'r2 CATCH ran' } }; s2() }
isa-ok r2(), Failure, 'a failing call in tail position returns its Failure';
