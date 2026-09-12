use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: static RegexTree nodes lower directly to the matcher
# plan.  Dynamic regex content remains on the established runtime parser path.

plan 11;

my $literal = EVAL(Q[/test/].AST);
ok 'test' ~~ $literal, 'a static literal lowered from RakuAST matches';
ok 'test' ~~ $literal, 'the same lowered literal matches a second time';
nok 'toast' ~~ $literal, 'the lowered literal still rejects a non-match';

my $digits = EVAL(Q[/\d+/].AST);
ok '12345' ~~ $digits, 'a quantified character class lowers to the matcher';
nok 'abc' ~~ $digits, 'the lowered character class rejects letters';

my $grammar = EVAL(Q[grammar GRegexTreeExecution { token digits { \d+ } }].AST);
ok $grammar.parse('123', :rule<digits>), 'a lowered token declaration matches';
ok $grammar.parse('456', :rule<digits>), 'a lowered token declaration matches again';
nok $grammar.parse('abc', :rule<digits>), 'the lowered token rejects a non-match';

my $rule-grammar = EVAL(Q[grammar GRegexTreeRuleExecution { rule pair { a[bc] d } }].AST);
ok $rule-grammar.parse('abc d', :rule<pair>),
    'a lowered rule keeps source adjacency while applying sigspace';
ok $rule-grammar.parse('abc d', :rule<pair>),
    'a lowered rule can apply its source tree a second time';
nok $rule-grammar.parse('abcd', :rule<pair>),
    'a lowered rule rejects a missing source whitespace boundary';
