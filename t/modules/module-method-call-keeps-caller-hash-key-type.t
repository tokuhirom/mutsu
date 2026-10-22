use lib 't/lib';
use Test;
use HashKeyLaneOwner;

plan 5;

# A module's method that stores into its file-scope `my %seen{List}` leaves
# the caller's same-named `my %seen{Any}` alone (#14595).
my %seen{Any};
lives-ok { %seen{{a => 0}} = 6 }, 'a Hash key before the call';
is HashKeyLaneOwner.new.put((1, 2)), 1, 'the module stores a List key into its own hash';
lives-ok { %seen{{a => 1}} = 6 }, 'a Hash key after the call';
lives-ok { %seen{(3, 4)} = 7 }, 'a List key after the call';
is %seen.elems, 3, "the caller's hash holds its own three entries";
