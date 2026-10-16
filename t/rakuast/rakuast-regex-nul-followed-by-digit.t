use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 5;

my $ast = Q[/\00/].AST;
my $body = $ast.statements[0].expression.body;
isa-ok $body, RakuAST::Regex::Sequence,
    'a NUL escape followed by a digit is a sequence';
isa-ok $body.terms[0], RakuAST::Regex::CharClass::Nul,
    'the escape remains the NUL character class';
isa-ok $body.terms[1], RakuAST::Regex::Literal,
    'the following digit remains a literal';
is $body.terms[1].text, '0', 'the literal digit is retained';
ok "\x[0]0" ~~ EVAL($ast), 'the constructed tree matches NUL followed by zero';
