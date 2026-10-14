use Test;

# An EVAL's undeclared-variable check sees the static scopes around the call,
# including lexicals declared later in their source (rakudo's pads are
# static), and so judges a variable in every position of the snippet (#10511).

plan 11;

{
    sub e($s) { EVAL $s }
    my $z = 4;
    is e('$z'), 4, 'a routine EVAL sees a lexical of its declaring block';
    is EVAL('$y.defined'), False, 'a direct EVAL sees a lexical declared later in its block';
    ok e('!$w.defined'), 'a routine EVAL sees a lexical declared later in its declaring block';
    my $y = 4;
    my $w = 5;
}

throws-like 'say 1 + $nope', X::Undeclared, 'an operator operand is judged';
throws-like 'if $nope { }', X::Undeclared, 'a condition is judged';
throws-like 'my $c = -> $a { $a + $nope }', X::Undeclared, 'a closure body is judged';
throws-like 'for 1 { say $nope }', X::Undeclared, 'a loop body is judged';

sub g { EVAL '$later-elsewhere' }
throws-like { g() }, X::Undeclared,
    'a lexical of an unrelated later scope is not in scope';
{ my $later-elsewhere = 1 }

eval-lives-ok 'BEGIN my $c = 1; say $c if 0', 'a statement-form BEGIN declares into its scope';
eval-lives-ok 'my $f = { say $^b if 0; $b }; $f(1)', 'a placeholder declares its bare name';
eval-lives-ok 'my $x = 1; say -$x + 2 if 0', 'a declared variable under operators is accepted';
