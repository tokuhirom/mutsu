use Test;
use lib 't/lib';
use RwForwardNamed;

plan 6;

# An `is rw` parameter of the caller, handed to an imported sub that also takes
# a named parameter, must see the callee's write (#16377).

sub w-named(str $t, $pos is rw) { bump-named($t, $pos) }
my $e = 0;
is w-named("5\n", $e), 5, "the imported sub's result comes back";
is $e, 1, "the callee's write reaches the caller's variable";
w-named("5\n", $e);
is $e, 2, "and again on the next call";

sub w-with-named($pos is rw) { bump-named("x", $pos, :z(1)) }
my $e2 = 0;
w-with-named($e2);
is $e2, 1, "with the named argument passed too";

sub w-plain(str $t, $pos is rw) { bump-plain($t, $pos) }
my $e3 = 0;
w-plain("x", $e3);
is $e3, 1, "an imported sub without a named parameter";

my $e4 = 0;
bump-named("x", $e4);
is $e4, 1, "a direct call";
