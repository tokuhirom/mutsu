use Test;

# A `constant` holding a Hash or List used as a parameter matches the argument
# by smartmatch (value), not by object identity. CSS::Properties::Calculator
# dispatches `calc(OpenParen, %expr, CloseParen)` on `my constant OpenParen =
# %( :op<(> )` (#14158). Expected values are the `raku` output.

plan 8;

my constant Open  = %( :op<(> );
my constant Close = %( :op<)> );
my constant L     = (1, 2);
my constant Five  = 5;

multi sub paren(Open, %e, Close) { "paren" }
multi sub paren(*@e) { "other" }
is paren({:op("(")}, {:a(1)}, {:op(")")}), 'paren', 'literal hashes match the constants';
my $h = ${:op("(")};
is paren($h, {:a(1)}, ${:op(")")}), 'paren', 'itemized hashes match';
my @l = ${:op("(")}, {:a(1)}, ${:op(")")};
is paren(|@l), 'paren', 'a slipped array matches';
is paren({:op("[")}, {:a(1)}, {:op(")")}), 'other', 'a different hash does not match';

multi sub lst(L) { "l" }
multi sub lst(*@e) { "other" }
is lst((1, 2)), 'l', 'a list constant matches an equal list';
is lst((1, 3)), 'other', 'and not a different one';

multi sub numc(Five) { "five" }
multi sub numc(*@e) { "other" }
is numc(5), 'five', 'a scalar constant still matches';
is numc(6), 'other', 'and still refuses another value';
