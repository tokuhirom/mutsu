use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 8;

my %arguments = primary => 'a';
my $ast = Q[/<word(%arguments<primary>)>/].AST;
my $expression = $ast.statements.head.expression;
isa-ok $expression, RakuAST::QuotedRegex, 'the source is a quoted regex';
ok $ast.gist.contains('RakuAST::Postcircumfix::LiteralHashIndex'),
    'the subrule argument retains the literal-hash-index node';
ok $ast.gist.contains('RakuAST::StrLiteral.new("primary")'),
    'the literal hash key remains a string literal';
my $regex = EVAL($ast);
ok $regex ~~ Regex, 'the quoted regex evaluates successfully';

grammar LiteralHashIndexArgument {
    token TOP { <word(%arguments<primary>)> }
    token word($expected) { $expected }
}

ok LiteralHashIndexArgument.parse('a').defined,
    'the literal hash-index argument reaches the subrule matcher';
%arguments<primary> = 'b';
ok LiteralHashIndexArgument.parse('b').defined,
    'the literal hash-index argument sees the current value after reassignment';
nok LiteralHashIndexArgument.parse('a').defined,
    'the literal hash-index argument no longer sees its earlier value';

my $expected = 'b';
grammar ExplicitValueArgument {
    token TOP { <word($expected)> }
    token word($value) { $value }
}
ok ExplicitValueArgument.parse('b').defined,
    'an explicit scalar argument keeps the established match-time behavior';
