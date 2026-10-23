use Test;

# A trailing colonpair after an infix application is an adverb of the operator:
# `1 + 2 :adverb` calls `infix:<+>(1, 2, :adverb)`. No core candidate declares a
# named parameter, so the call is rejected instead of dropping the adverb, in a
# statement, an assignment and a listop argument alike. Measured on rakudo
# 2026.09.

plan 12;

sub rejected(&code) {
    try code();
    $!.^name eq 'X::Multi::NoMatch' ?? $!.message.lines[0] !! "no error: {$!.^name}"
}

is rejected({ 1 + 2 :adverb }), 'Cannot resolve caller infix:<+>(Int:D, Int:D, :adverb); none of these signatures matches:', 'an adverb on `+`';
is rejected({ 1 ~ 2 :adverb }), 'Cannot resolve caller infix:<~>(Int:D, Int:D, :adverb); none of these signatures matches:', 'an adverb on `~`';
is rejected({ 1 & 2 :adverb }), 'Cannot resolve caller infix:<&>(Int:D, Int:D, :adverb); none of these signatures matches:', 'an adverb on a junction operator';
is rejected({ my $x = 1 + 2 :adverb }), 'Cannot resolve caller infix:<+>(Int:D, Int:D, :adverb); none of these signatures matches:', 'on the right of an assignment';
is rejected({ my @a = 1 + 2 :adverb }), 'Cannot resolve caller infix:<+>(Int:D, Int:D, :adverb); none of these signatures matches:', 'in a list assignment';
is rejected({ EVAL 'say 1 + 2 :adverb' }), 'Cannot resolve caller infix:<+>(Int:D, Int:D, :adverb); none of these signatures matches:', 'as an argument of a listop';

# A user operator receives the adverb.
{
    my sub infix:<plus>($a, $b, :$x) { "$a+$b x=$x" }
    is (1 plus 2 :x(3)), '1+2 x=3', 'a user operator takes the adverb';
}

# A plain trailing named argument of the listop itself is untouched.
sub f($a, :$n) { "$a $n" }
is f(1, :n(2)), '1 2', 'a call keeps its own named argument';

# The source form round-trips through RakuAST as an application with an adverb.
my $e = '1 + 2 :adverb'.AST.statements[0].expression;
is $e.^name, 'RakuAST::ApplyInfix', 'the operator application is an ApplyInfix';
is $e.infix.operator, '+', 'its infix is `+`';
my $l = '1 | 2 | 3 :x'.AST.statements[0].expression;
is $l.^name, 'RakuAST::ApplyListInfix', 'a junction chain is an ApplyListInfix';
is $l.operands.elems, 3, 'holding every operand';
