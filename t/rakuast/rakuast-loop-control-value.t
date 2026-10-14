use v6.e.PREVIEW;
use Test;
use experimental :rakuast;

# A loop control is a bare `Call::Name::WithoutParentheses` in rakudo 2026.09
# whatever its argument: a label (`next L`, a `Term::Name`), a value
# (`last $_ * 10`, v6.e) or a slip (`next |$c`). Lowering the statement form
# must tell the label from the value the way the parser does.
plan 11;

# `.AST` parses its string as a unit of its own, so each string asks for v6.e.
sub ast(Str $source) { "use v6.e.PREVIEW;\n$source".AST }
sub first-in-loop(Str $source) {
    ast($source).statements[1].body.body.statement-list.statements[0].expression
}

# --- read direction ---------------------------------------------------------

my $value = first-in-loop(Q[for ^5 { last $_ * 10 if $_ == 2 }]);
is $value.^name, 'RakuAST::Call::Name::WithoutParentheses', '`last VALUE` is a bare call';
is $value.args.args[0].^name, 'RakuAST::ApplyInfix', 'whose argument is the value';

my $label = first-in-loop(Q[L: for ^2 { next L }]);
is $label.args.args[0].^name, 'RakuAST::Term::Name', '`next LABEL` names the label as a term';

# --- write direction (EVAL) and semantics -----------------------------------

is-deeply ast(Q[(do for ^5 { last $_ * 10 if $_ == 2; $_ }).List]).EVAL, (0, 1, 20),
    'a round-tripped `last VALUE` ends the loop with the value';
is-deeply ast(Q[(for ^3 { next Empty }).List]).EVAL, (),
    'a round-tripped `next Empty` is a value, not a label';
is ast(Q[my $n = 0; LP: for ^3 { for ^3 { $n++; next LP } }; $n]).EVAL, 3,
    'a round-tripped `next LABEL` still names the loop';

is-deeply (do for ^5 { last $_ * 10 if $_ == 2; $_ }).List, (0, 1, 20),
    '`last VALUE` as a statement';
is-deeply (for ^3 { next Empty }).List, (), '`next Empty` contributes nothing';
my @seen;
for ^2 -> $i { my $c = \(); @seen.push($i); next |$c }
is-deeply @seen, [0, 1], '`next |$c` with an empty capture is a plain next';
my $n = 0;
LBL: for ^3 { for ^3 { $n++; my $c = \(LBL); last |$c } }
is $n, 1, '`last |$c` over a captured label leaves the labelled loop';
my $m = 0;
LQ: for ^3 { for ^3 { $m++; next LQ } }
is $m, 3, '`next LABEL` is still the labelled form';
