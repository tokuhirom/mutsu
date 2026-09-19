use v6.d;
use Test;

plan 2;

my $csv = q{"a","b","c"};

is $csv.subst(/ ^ '"' | '"' $/, :g), 'a","b","c',
    'anchors stay local to their alternation branches during subst';

my @matches = $csv.match(/ ^ '"' | '"' $/, :g);
is-deeply @matches».from, [0, 10],
    'an anchored alternation matches only the subject boundaries';
