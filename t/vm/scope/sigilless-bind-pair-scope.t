use Test;

# A `my \h = k` declaration inside a routine records a slot-to-slot bind pair
# so writes to one name reach the other. The pair holds slot indices of the
# routine's own frame: it must not survive the call. The light and fast call
# paths used to leave it behind, so the caller's slots with the same indices
# became bound to each other (and every later store scanned one stale pair per
# earlier call, which made a loop of such calls quadratic).

plan 5;

sub f() { my \k = 5; my \h = k; h }

sub g() { my $a = 1; my $b = 2; f(); $a = 10; $b }
is g(), 2, 'a callee bind pair does not bind the caller routine slots';

my $x = 1;
my $y = 2;
f();
$x = 10;
is $y, 2, 'a callee bind pair does not bind the mainline slots';
is $x, 10, 'the assigned variable itself is written';

sub h() {
    my $p = 1;
    my \q = $p;
    $p = 7;
    q
}
is h(), 7, 'a bind inside the routine still tracks its source';

my $sum = 0;
$sum += f() for ^2000;
is $sum, 10000, 'repeated calls keep binding correctly';
