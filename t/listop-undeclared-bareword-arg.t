use Test;

plan 8;

# An undeclared bareword followed by another bareword term is a no-paren list-op
# call: `color White` parses structurally as `color(White)` (both are then
# reported as undeclared at the semantic stage, exactly as raku does). Before
# this fix mutsu failed to parse it inside parens / colonpairs with a structural
# "Confused" error; now it parses and raises X::Undeclared::Symbols like raku.

throws-like 'my $x = (color White)', X::Undeclared::Symbols,
    'bareword listop arg inside parens parses structurally (undeclared, not Confused)';

throws-like 'my %h = :fill-color(color White)', X::Undeclared::Symbols,
    'bareword listop arg inside a colonpair parses structurally';

throws-like 'color White', X::Undeclared::Symbols,
    'bareword listop arg at statement level parses as a call';

# A declared listop takes the following bareword term as an argument the same
# way (here the argument is a declared constant so raku accepts it too).
sub tint($c) { "tint:$c" }
constant Tan = 'tan';
my %h = :fill-color(tint Tan);
is %h<fill-color>, 'tint:tan', 'declared listop gobbles a bareword arg inside a colonpair';

# Regression guards: infix word operators after a bareword keep their infix
# reading and are NOT gobbled as list-op arguments.
is (1 eqv 1), True, 'eqv stays an infix operator, not a listop argument';
is ('a' x 3), 'aaa', 'x stays the infix repeat operator';

# Regression guard: declarator keywords (`method foo {}`) are not list-op heads.
my $m;
class C { $m = method foo {}; }
isa-ok $m, Method, 'method-expression is a declarator, not a listop head';
is $m.name, 'foo', 'method-expression name is visible';
