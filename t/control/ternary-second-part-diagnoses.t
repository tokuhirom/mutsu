use Test;

# rakudo has several distinct compile-time diagnoses for the ways a ternary
# `?? !!`'s then-branch can fail to reach a `!!`: `::`/`:` typoed for `!!`
# (X::Syntax::ConditionalOperator::SecondPartInvalid), a colonpair adverb or
# the comma list separator sitting at a precedence looser than `?? !!`
# (X::Syntax::ConditionalOperator::PrecedenceTooLoose), and a bareword whose
# listop call swallows the `!!` as one of its own arguments
# (X::Syntax::ConditionalOperator::SecondPartGobbled -- this happens even for
# an UNDECLARED bareword, since rakudo always tries the listop-call parse).
# mutsu previously collapsed all of these into the generic
# X::Syntax::Confused. Also covers the related X::Syntax::Adverb gap this
# fix's paren-parsing change closed: a colonpair immediately following a bare
# literal (`(3 :foo)`).
#
# Every assertion here is verified byte-identical against `raku -e '...'`.

plan 18;

try { EVAL '1 ?? 2,3 !! 4,5' };
is $!.^name, 'X::Syntax::ConditionalOperator::PrecedenceTooLoose',
    'a comma in the then-branch is PrecedenceTooLoose';
is $!.message, 'Precedence of , is too loose to use inside ?? !!; please parenthesize',
    'comma PrecedenceTooLoose message matches rakudo';
is $!.operator, ',', 'comma PrecedenceTooLoose .operator is the comma';

try { EVAL '1 ?? 3 :: 2' };
is $!.^name, 'X::Syntax::ConditionalOperator::SecondPartInvalid',
    ':: instead of !! is SecondPartInvalid';
is $!.message, 'Please use !! rather than ::', ':: message matches rakudo';
is $!.second-part, '::', ':: .second-part is "::"';

try { EVAL '1 ?? 3 : 2' };
is $!.^name, 'X::Syntax::ConditionalOperator::SecondPartInvalid',
    'a bare : instead of !! is SecondPartInvalid';
is $!.second-part, ':', ': .second-part is ":"';

try { EVAL '1 ?? 3 :foo :: 2' };
is $!.^name, 'X::Syntax::ConditionalOperator::PrecedenceTooLoose',
    'an adverb in the then-branch is PrecedenceTooLoose';
is $!.operator, ':foo', 'adverb PrecedenceTooLoose .operator is the spelled adverb';

try {
    my @x = ^10;
    my @y = 2..3;
    EVAL 'my @z = @y ?? @x[@y] :v !! @x';
}
is $!.^name, 'X::Syntax::ConditionalOperator::PrecedenceTooLoose',
    'an adverb on a subscript in the then-branch is also PrecedenceTooLoose';
is $!.operator, ':v', 'subscript adverb .operator is ":v"';

try { EVAL 'sub rt123115 { 2 }; 1 ?? rt123115 !! 3' };
is $!.^name, 'X::Syntax::ConditionalOperator::SecondPartGobbled',
    'a declared-sub bareword in then-position that swallows !! is SecondPartGobbled';
is $!.message, 'Your !! was gobbled by the expression in the middle; please parenthesize',
    'SecondPartGobbled message matches rakudo';

try { EVAL '1 ?? b !! 2' };
is $!.^name, 'X::Syntax::ConditionalOperator::SecondPartGobbled',
    'an UNDECLARED bareword also gobbles -- rakudo always tries the listop-call parse';

# A literal backslash-n (NOT a newline -- single-quoted, so \n is two chars)
# glued directly onto the bareword with no separating whitespace is bogus
# code, not a clean listop-call gobble, so this stays the generic Confused.
try { EVAL '1 ?? b\n !! 2' };
is $!.^name, 'X::Syntax::Confused',
    'a bareword immediately followed by non-whitespace garbage is still the generic Confused';

try { EVAL '(3 :foo)' };
is $!.^name, 'X::Syntax::Adverb',
    'a colonpair directly on a bare literal is X::Syntax::Adverb';
is $!.message, q{You can't adverb 3}, 'the message names the literal itself';
