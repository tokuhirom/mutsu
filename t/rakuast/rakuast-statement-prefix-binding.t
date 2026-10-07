use v6;
use Test;

# `try STATEMENT if COND` holds the modifier inside the try, and `sink A, B`
# sinks the whole comma list (#12240). Passes under BOTH mutsu and raku.

plan 7;

my $c = 1;
sub foo { 1 }

my $try = Q[try foo() if $c].AST.statements[0].expression;
is $try.^name, 'RakuAST::StatementPrefix::Try', 'try is the outer expression';
my $inner = $try.blorst;
is $inner.^name, 'RakuAST::Statement::Expression', 'try holds a statement';
is $inner.condition-modifier.^name, 'RakuAST::StatementModifier::If', 'the if modifier is inside the try';

my $sink = Q[sink 1, 2].AST.statements[0].expression;
is $sink.^name, 'RakuAST::StatementPrefix::Sink', 'sink is the outer expression';
is $sink.blorst.expression.^name, 'RakuAST::ApplyListInfix', 'sink covers the whole comma list';

my $i = 0;
sink $i++, $i++;
is $i, 2, 'sink A, B evaluates both operands';

my @seen;
try (@seen.push($_); die "x") for 1..3;
is @seen.elems, 1, 'a for modifier loops inside the try';
