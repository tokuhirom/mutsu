use Test;

plan 7;

my regex prefix_optional ($has-prefix is rw) {
    { $has-prefix = False }
    [ <alpha>+ { $has-prefix = True } ]?
    \d+
}
my $seen;
'123' ~~ / <prefix_optional: $seen> /;
is $seen.raku, 'Bool::False', 'the rule writes the caller variable (no prefix)';
'abc456' ~~ / <prefix_optional: $seen> /;
is $seen.raku, 'Bool::True', 'the rule writes the caller variable (prefix)';

my regex twice ($n is rw) { \d+ { $n = $/.Str.Int * 2 } }
my $v = 0;
'21' ~~ / <twice: $v> /;
is $v, 42, 'a numeric value written through the parameter';

my regex word ($s is rw) { <alpha>+ { $s = ~$/ } }
sub f { my $x; 'abc' ~~ / <word: $x> /; $x }
is f(), 'abc', 'a sub-local variable is the aliased one';

my $untouched = 'keep';
'abc' ~~ / <word: $untouched> /;
is $untouched, 'abc', 'an existing value is overwritten';

throws-like { '7' ~~ / <twice: 5> / }, X::Parameter::RW,
    'a literal argument for an is rw parameter is X::Parameter::RW';

my $k = 1;
'x' ~~ / <word: $k> /;
is $k, 'x', 'the variable is usable after the match';
