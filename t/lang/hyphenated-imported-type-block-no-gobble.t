use v6;
use lib 't/lib';
use Test;
use Hy-Phen::Thing;

# A hyphenated *qualified* type name imported from another module
# (`Hy-Phen::Thing`) must not gobble a following `{ ... }` block as a call
# argument. At parse time the imported type is not yet known to the parser's
# user-type table, so the old heuristic treated any hyphenated unknown name as a
# forward-referenced sub and consumed the block — breaking
# `$x ~~ Hy-Phen::Thing { ... }` (the `{ ... }` is the if/unless/when body).
# The short name starts with an uppercase letter, so it is a type, not a sub.
# Regression seen in PB-Lottery (`$ticket ~~ PB-Lottery::Ticket { ... }`).

plan 5;

my $t = Hy-Phen::Thing.new(:v(3));

# smartmatch against the imported hyphenated type; block is the if-body
if $t ~~ Hy-Phen::Thing {
    pass "if-block belongs to the if, not gobbled by Hy-Phen::Thing";
} else {
    flunk "instance should smartmatch its own type";
}

# unless variant
unless 5 ~~ Hy-Phen::Thing {
    pass "unless-block belongs to the unless, not gobbled";
}

# given/when
my $branch = 'none';
given $t {
    when Hy-Phen::Thing { $branch = 'matched' }
    default             { $branch = 'default' }
}
is $branch, 'matched', "when Hy-Phen::Thing block is the when-body";

# the accessor works (proves the type really loaded and constructed)
is $t.v, 3, "imported hyphenated type constructs and accesses attributes";

# A lowercase hyphenated forward-referenced sub still gobbles its block arg.
my $ran = 0;
run-it { $ran = 1 };
is $ran, 1, "lowercase hyphenated forward-ref sub still gobbles its block";
sub run-it(&b) { b() }
