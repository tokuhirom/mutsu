use Test;

plan 5;

# A `-->` return type belongs to the routine that declared it and is never
# inherited lexically. A closure created inside such a routine captured the
# enclosing `__mutsu_return_type` and had ITS OWN return checked against it —
# which only showed up when the routine was invoked through a Callable value
# (Digest::SHA1's `reduce &sha1-block, …`).

sub inner-block-return(blob32 $H, blob32 $M --> blob32) {
    blob32.new: $H Z+ (
        reduce -> blob32 $b, $i {
            blob32.new:
                ({ $^a + $^b + $^c }, { $^a * $^b * $^c })[$i % 2](|$b[1..3]),
                $b[0], $b[1], $b[2], $b[3]
        }, $H, |^3
    )
}

my $H = blob32.new(1, 2, 3, 4, 5);
my $M = blob32.new(1 xx 16);

is inner-block-return($H, $M).list, (13, 8, 12, 5, 7), 'called by name';

my &f = &inner-block-return;
is f($H, $M).list, (13, 8, 12, 5, 7), 'called through a Callable variable';
is (&inner-block-return)($H, $M).list, (13, 8, 12, 5, 7), 'called through a code literal';
is (reduce &inner-block-return, $H, $M).list, (13, 8, 12, 5, 7), 'called by reduce';

# The routine's own return type is still enforced.
sub bad-return(--> Int) { "nope" }
dies-ok { bad-return() }, 'the declaring routine still enforces its return type';
