use Test;

# Found through EERPG 0.0.2 (t/Action.t): `postfix:<@> is looser(&infix:<->)`
# and `amount => -1@`. A postfix declared looser than a built-in prefix applies
# to the whole prefix expression: `-1@` is `(-1)@`. A postfix without a
# precedence trait stays tighter: `-1§` is `-(1§)`.

plan 12;

sub postfix:<@>(\a) is looser(&infix:<->) { "<{a.raku}>" }
sub postfix:<§>(\a) { "<{a}>" }
sub postfix:<¤>(\a) is tighter(&infix:<+>) { "<{a.raku}>" }
my $x = 3;

is -1@,       '<-1>',   'literal operand';
is - 1@,      '<-1>',   'whitespace after the prefix';
is -$x@,      '<-3>',   'variable operand';
is -$x.abs@,  '<-3>',   'method call in the operand';
is -(1)@,     '<-1>',   'parenthesised operand';
is +"5"@,     '<5>',    'numeric prefix';
is ~5@,       '<"5">',  'string prefix';
is !$x@,      '<Bool::False>', 'boolean prefix';
is [1, -2@].elems, 2,   'inside a list';
is -1¤,       '<-1>',   'tighter(+) is still looser than prefix minus';
is 5@,        '<5>',    'no prefix at all';
is (try EVAL('-1§')).defined, False, 'a tight postfix applies first: -("<1>") is not numeric';

