use experimental :rakuast;
use Test;

# `:5($n)`, a radix conversion written with parentheses, is a
# `RakuAST::Term::RadixNumber` (#14880, the `.DEPARSE` corpus's
# integration/advent2013-day15.t). mutsu runs it as `UNBASE(5, $n)`.

plan 12;

sub last-expr($src) { $src.AST.statements[*-1].expression }

is Q{my $n = "42"; :5($n)}.AST.statements[*-1].DEPARSE.chomp, ':5($n)', '.DEPARSE of :5($n)';
is Q{:16("ff")}.AST.statements[0].DEPARSE.chomp, ':16("ff")', 'of :16("ff")';
is Q{say :2("1010") + 1}.AST.statements[0].DEPARSE.chomp, 'say :2("1010") + 1', 'inside an expression';

my $node = last-expr(Q{my $n; :5($n)});
isa-ok $node, RakuAST::Term::RadixNumber, 'the node class';
is $node.radix, 5, '.radix';
is $node.multi-part, False, '.multi-part';
isa-ok $node.value, RakuAST::Circumfix::Parentheses, '.value is the parenthesized expression';

my $n = "42";
is :5($n), 22, 'the conversion runs';
is EVAL(Q{my $m = "42"; :5($m)}.AST), 22, 'EVAL of the .AST runs it';

my $built = RakuAST::Term::RadixNumber.new(
    radix => 16,
    value => RakuAST::Circumfix::Parentheses.new(RakuAST::SemiList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::StrLiteral.new("ff")))));
is $built.DEPARSE, ':16("ff")', 'a constructed node deparses';
is EVAL($built), 255, 'and runs';
is $built.multi-part, False, 'multi-part defaults to False';
