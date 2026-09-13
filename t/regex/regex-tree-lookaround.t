use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: explicit static lookaround assertions retain their
# source tree and lower through the existing RegexPattern matcher.

plan 15;

my $positive-before = EVAL(Q[/foo <?before bar>/].AST);
my $before-match = 'foobar' ~~ $positive-before;
ok $before-match, 'a positive lookahead matches its suffix';
is ~$before-match, 'foo', 'a positive lookahead remains zero-width';
ok 'foobar' ~~ $positive-before,
    'a positive lookahead can be reused after lowering';
nok 'foobaz' ~~ $positive-before,
    'a positive lookahead rejects a missing suffix';

my $negative-before = EVAL(Q[/foo <!before bar>/].AST);
ok 'foobaz' ~~ $negative-before,
    'a negative lookahead accepts a different suffix';
nok 'foobar' ~~ $negative-before,
    'a negative lookahead rejects its suffix';

my $positive-after = EVAL(Q[/<?after foo> bar/].AST);
my $after-match = 'foobar' ~~ $positive-after;
ok $after-match, 'a positive lookbehind matches its prefix';
is ~$after-match, 'bar', 'a positive lookbehind remains zero-width';
nok 'bazbar' ~~ $positive-after,
    'a positive lookbehind rejects a different prefix';

my $negative-after = EVAL(Q[/<!after foo> bar/].AST);
ok 'bazbar' ~~ $negative-after,
    'a negative lookbehind accepts a different prefix';
nok 'foobar' ~~ $negative-after,
    'a negative lookbehind rejects its prefix';

my $constructed = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('foo'),
        RakuAST::Regex::Assertion::Lookahead.new(
            assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                name => RakuAST::Name.from-identifier('before'),
                regex-arg => RakuAST::Regex::Literal.new('bar'),
            ),
        ),
    ),
));
ok 'foobar' ~~ $constructed,
    'constructed lookaround nodes lower through EVAL';

my $grammar = EVAL(Q[grammar GRegexTreeLookaround {
    token TOP { foo <?before bar> bar }
}].AST);
ok $grammar.parse('foobar'),
    'a token declaration lowers a lookahead through its retained source tree';
ok $grammar.parse('foobar'),
    'a lowered declaration can reuse its lookahead without duplicate policy';
nok $grammar.parse('foobaz'),
    'a declaration lookahead rejects the wrong suffix';
