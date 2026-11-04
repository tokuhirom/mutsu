use v6;
use Test;

plan 6;

# A bareword naming a routine the unit declares is resolved by the bareword
# classifier and lowered where the parser built it (ADR-15760 slice 2).

sub die-on-fail { 7 }
is die-on-fail, 7, 'a hyphenated declared routine is called without parentheses';
my $v = die-on-fail;
is $v, 7, 'it is a term in an initializer';

sub w { 'a' }
is (w) ~ (w), 'aa', 'a declared routine word is a term in a concatenation';

my @got;
react {
    whenever supply { emit w; emit w; done } { @got.push($_) }
}
is @got.join, 'aa', 'a declared routine word inside a supply body';

sub shadow { 'declared' }
is shadow, 'declared', 'a lowercase declared word is a call';

is EVAL('sub zz { 3 }; (zz) * 2'), 6, 'a declared routine word in a string EVAL';
