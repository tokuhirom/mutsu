use Test;
use experimental :rakuast;

plan 4;

# A hand-built node has no origin: rakudo answers the undefined RakuAST::Origin
# (a type object), which `with` skips like Nil.
my $n = RakuAST::IntLiteral.new(5);
is $n.origin.^name, 'RakuAST::Origin', 'a hand-built node: the Origin type object';
nok $n.origin.defined, '... which is not defined';
my $visited = 0;
with $n.origin { $visited++ }
is $visited, 0, '... so `with` skips it';

my $parsed = "5 + 6;".AST.statements[0].expression;
ok $parsed.origin.defined, 'a parsed node has a defined origin';
