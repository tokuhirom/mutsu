use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: an ordinary method call in a subrule argument keeps
# its RakuAST expression tree and can be lowered back to the regex parser.

plan 7;

my $value = 'a';
my $ast = Q[/<word($value.uc)>/].AST;
my $gist = $ast.gist;
ok $gist.contains('RakuAST::Regex::Assertion::Named::Args'),
    'the dynamic argument remains an argumented subrule';
ok $gist.contains('RakuAST::ApplyPostfix'),
    'the method call remains a postfix application';
ok $gist.contains('RakuAST::Call::Method'),
    'the method call keeps its method-call node';
ok $gist.contains('RakuAST::Name.from-identifier("uc")'),
    'the method name remains accessible in the argument tree';

my $regex = EVAL($ast);
ok $regex ~~ Regex,
    'a RakuAST regex with a method-call argument lowers successfully';

grammar GDynamicMethodArgument {
    token TOP { <word($value.uc)> }
    token word($expected) { $expected }
}
ok GDynamicMethodArgument.parse('A').defined,
    'a method-call argument reaches the subrule matcher';
$value = 'b';
ok GDynamicMethodArgument.parse('B').defined,
    'the dynamic method-call argument observes reassignment at match time';
