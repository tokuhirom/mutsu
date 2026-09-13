use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: source-level anchor nodes lower through the existing
# RegexPattern matcher without changing the Parser -> Compiler -> VM path.

plan 10;

my $start = EVAL(Q[/^a/].AST);
ok 'a' ~~ $start, 'a start-of-string anchor matches at the subject start';
nok 'ba' ~~ $start, 'a start-of-string anchor rejects a later occurrence';

my $end = EVAL(Q[/a$/].AST);
ok 'ba' ~~ $end, 'an end-of-string anchor matches at the subject end';
nok 'ab' ~~ $end, 'an end-of-string anchor rejects a non-final occurrence';

my $line-start = EVAL(Q[/^^a/].AST);
ok "b\na" ~~ $line-start, 'a start-of-line anchor matches after a newline';
nok "b\nc" ~~ $line-start, 'a start-of-line anchor rejects a non-matching line';

my $line-end = EVAL(Q[/a$$/].AST);
ok "b\na" ~~ $line-end, 'an end-of-line anchor matches before a line ending';
nok "ab\nc" ~~ $line-end, 'an end-of-line anchor rejects a non-final line';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Anchor::BeginningOfString.new,
        RakuAST::Regex::Literal.new('a'),
        RakuAST::Regex::Anchor::EndOfString.new,
    ),
));
ok 'a' ~~ $constructed, 'constructed anchor nodes lower through EVAL';
nok 'ba' ~~ $constructed, 'constructed anchors retain their execution policy';
