use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 13;

my $values = Q["f fo foo fooo" ~~ m:nth(2, 3)/fo+/].AST;
my $adverb = $values.statements[0].expression.right.adverbs[0];
isa-ok $adverb, RakuAST::ColonPair::Value,
    'a list-valued counted modifier remains a colonpair value';
is $adverb.key, 'nth', 'the counted modifier name is retained';
isa-ok $adverb.value, RakuAST::Circumfix::Parentheses,
ok $adverb.value.gist.contains('RakuAST::ApplyListInfix'),
    'the comma list remains an infix expression';
is EVAL($values).list.map(*.Str).join('|'), 'foo|fooo',
    'a comma-list counted modifier round-trips';

my $last = Q["ABCDE" ~~ m:nth(*)/\w/].AST;
ok $last.gist.contains('RakuAST::Term::Whatever'),
    'a Whatever counted argument remains a Whatever term';
is EVAL($last).Str, 'E', 'a Whatever counted argument selects the last match';

my $previous = Q["ABCDE" ~~ m:nth(*-1)/\w/].AST;
ok $previous.gist.contains('RakuAST::ApplyInfix'),
    'a Whatever calculation remains an infix expression';
is EVAL($previous).Str, 'D', 'a Whatever calculation selects from the end';

my $numeric = Q["¼½3" ~~ /<:NumericValue(0 ^..^ 1)>+/].AST;
ok $numeric.gist.contains('property  => "NumericValue"'),
    'the numeric property name remains in the regex tree';
ok $numeric.gist.contains('RakuAST::Infix.new("^..^")'),
    'the Unicode property predicate remains an expression';
is EVAL($numeric).Str, '¼½',
    'the numeric property expression round-trips';

my $name = Q["FooBar" ~~ /<:name(/:s LATIN SMALL LETTER/)>+/].AST;
is EVAL($name).Str, 'oo',
    'a regex-valued Unicode name predicate round-trips';
