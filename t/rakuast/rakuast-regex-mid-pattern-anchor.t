use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 5;

my $ast = Q[/z^abc/].AST;
my $body = $ast.statements[0].expression.body;
is $body.terms.map(*.^name).join(' '),
  'RakuAST::Regex::Literal RakuAST::Regex::Anchor::BeginningOfString RakuAST::Regex::Literal',
  'a mid-pattern ^ remains a beginning-of-string anchor node';
is $body.terms[0].text, 'z', 'the preceding literal stays before the anchor';
is $body.terms[2].text, 'abc', 'the following literal stays after the anchor';

my $constructed = RakuAST::QuotedRegex.new(body => RakuAST::Regex::Sequence.new(
  RakuAST::Regex::Literal.new('z'),
  RakuAST::Regex::Anchor::BeginningOfString.new,
  RakuAST::Regex::Literal.new('abc')));
nok 'zabc' ~~ EVAL($constructed),
  'EVAL keeps the anchor at the beginning of the string after a preceding atom';
ok 'abc' ~~ EVAL(Q[/<?>^abc/].AST),
  'a zero-width atom before ^ still matches at the start of the string';
