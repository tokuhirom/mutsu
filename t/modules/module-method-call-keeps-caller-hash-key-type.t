use lib 't/lib';
use Test;
use HashKeyLaneOwner;

plan 5;

# A module's method that stores into its file-scope `my %seen{List}` leaves
# the caller's same-named `my %seen{Any}` alone (#14595).
my %seen{Any};
my $h0 = {a => 0};
my $h1 = {a => 1};
my $l = (3, 4);
lives-ok { %seen{$h0} = 6 }, 'a Hash key before the call';
is HashKeyLaneOwner.new.put((1, 2)), 1, 'the module stores a List key into its own hash';
lives-ok { %seen{$h1} = 6 }, 'a Hash key after the call';
lives-ok { %seen{$l} = 7 }, 'a List key after the call';
is %seen.elems, 3, "the caller's hash holds its own three entries";
