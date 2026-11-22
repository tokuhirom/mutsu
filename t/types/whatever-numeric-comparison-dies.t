use Test;

# #18766: a `Whatever` *value* has no `.Numeric` / `.Real` candidate, so a
# numeric comparison with one dies as rakudo's does; a `*` written as an
# operand still curries.

plan 9;

my $w = *;
throws-like { $w == 1 }, X::Multi::NoMatch, message => /'Numeric(Whatever:D: )'/, '==';
throws-like { 1 != $w }, X::Multi::NoMatch, message => /'Numeric(Whatever:D: )'/, '!= on the right';
throws-like { $w < 1 }, X::Multi::NoMatch, message => /'Real(Whatever:D: )'/, '< coerces through Real';
throws-like { $w <=> 1 }, X::Multi::NoMatch, message => /'Real(Whatever:D: )'/, '<=>';
my $hw = **;
throws-like { $hw == 1 }, X::Multi::NoMatch, message => /'Numeric(HyperWhatever:D: )'/, 'HyperWhatever';

ok (* == 1)(1), '* == 1 still curries';
ok (1 < * < 3)(2), 'a chained comparison still curries';
is-deeply (1..5).grep(1 < * < 4), (2, 3), '...and filters';
nok "foo" ~~ *.chars == 3, 'a smartmatch-primed chain is unchanged';
