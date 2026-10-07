use v6;
use experimental :rakuast;
use Test;

# `STMT when COND` is a statement with a `StatementModifier::When` condition
# modifier, not a `when` clause (measured on rakudo 2026.09). The tree EVALs to
# the plain conditional rakudo lowers it to: a false modifier yields `Empty`,
# and it does not leave the enclosing block.

plan 7;

sub text($node) { $node.raku.lines.map(*.trim).join(' ') }
my $node = '1 when 2'.AST.statements[0];

is $node.^name, 'RakuAST::Statement::Expression', 'the modified statement stays an expression';
is text($node.condition-modifier),
    'RakuAST::StatementModifier::When.new( RakuAST::IntLiteral.new(2) )',
    'the condition hangs off it as a StatementModifier::When';

is Q|my $m = 'abc' ~~ /b/; ('x' when $m)|.AST.EVAL, 'x', 'a true modifier yields its statement';
is-deeply Q|('Boo!' when /ghost/ given 'phantom')|.AST.EVAL, Empty, 'a false modifier yields Empty';
is Q|my $r; given 'camelia' { $r = 'bug' when /camelia/ }; $r|.AST.EVAL, 'bug',
    'the topic of the enclosing given is the modifier topic';
is Q[my @r; for 1..4 { @r.push($_) when 2 | 3 }; @r.join(',')].AST.EVAL, '2,3',
    'a modifier when in a loop body filters like an if';
is Q|('Boo!' when /phantom/ given 'phantom')|.AST.EVAL, 'Boo!', 'when ... given ... chains';
