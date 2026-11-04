use v6;
use Test;

# Regression (Sub::Memoized, #16326): a closure given as a custom routine
# trait argument must update the outer lexical itself, not a by-value copy
# taken when the hoisted sub declaration is registered.

plan 3;

my $seen = 0;
my int $count;
my %h;
my @keep;
multi sub trait_mod:<is>(Routine:D $r, :$mm!) { @keep.push($mm) }
sub bar($a) is mm({ ++$seen; ++$count; %h<k> = 7; 1 }) { $a }

@keep[0]() for ^3;
is $seen, 3, 'scalar lexical written by the trait closure';
is $count, 3, 'native int lexical written by the trait closure';
is %h<k>, 7, 'hash lexical written by the trait closure';
