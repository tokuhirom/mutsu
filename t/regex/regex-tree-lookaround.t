use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: explicit static lookaround assertions retain their
# source tree and lower through the existing RegexPattern matcher.

plan 32;

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

my $bare-before = EVAL(Q[/foo <before bar>/].AST);
my $bare-before-match = 'foobar' ~~ $bare-before;
ok $bare-before-match,
    'an unprefixed lookahead matches through the shared tree';
is $bare-before-match<before>.from, 3,
    'an unprefixed lookahead captures its zero-width position';
is $bare-before-match<before>.to, 3,
    'an unprefixed lookahead capture does not include the asserted body';
nok 'foobaz' ~~ $bare-before,
    'an unprefixed lookahead rejects a missing suffix';

my $dot-before = EVAL(Q[/foo <.before bar>/].AST);
my $dot-before-match = 'foobar' ~~ $dot-before;
ok $dot-before-match,
    'a dot-suppressed lookahead matches through the shared tree';
nok $dot-before-match<before>.defined,
    'a dot-suppressed lookahead does not publish a named capture';

my $bare-after = EVAL(Q[/foo <after foo>/].AST);
my $bare-after-match = 'foobar' ~~ $bare-after;
ok $bare-after-match,
    'an unprefixed lookbehind matches through the shared tree';
is $bare-after-match<after>.from, 3,
    'an unprefixed lookbehind captures its zero-width position';
is $bare-after-match<after>.to, 3,
    'an unprefixed lookbehind capture does not include the asserted body';

my $dot-after = EVAL(Q[/foo <.after foo>/].AST);
my $dot-after-match = 'foobar' ~~ $dot-after;
ok $dot-after-match,
    'a dot-suppressed lookbehind matches through the shared tree';
nok $dot-after-match<after>.defined,
    'a dot-suppressed lookbehind does not publish a named capture';

my $constructed-bare = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Named::RegexArg.new(
        name => RakuAST::Name.from-identifier('before'),
        regex-arg => RakuAST::Regex::Literal.new('bar'),
        capturing => True,
    ),
));
my $constructed-bare-match = 'bar' ~~ $constructed-bare;
ok $constructed-bare-match,
    'a constructed named lookaround lowers through EVAL';
is $constructed-bare-match<before>.from, 0,
    'a constructed named lookaround preserves its capture policy';

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

my $nested = EVAL(Q[/foo <?before [<?before bar>]> bar/].AST);
ok 'foobar' ~~ $nested,
    'a nested static lookahead lowers through the shared tree';
ok 'foobar' ~~ $nested,
    'a nested static lookahead can be reused without reparsing drift';
nok 'foobaz' ~~ $nested,
    'a nested static lookahead rejects a different suffix';

my $constructed-nested = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Lookahead.new(
        assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
            name => RakuAST::Name.from-identifier('before'),
            regex-arg => RakuAST::Regex::Group.new(
                RakuAST::Regex::Assertion::Lookahead.new(
                    assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                        name => RakuAST::Name.from-identifier('before'),
                        regex-arg => RakuAST::Regex::Literal.new('bar'),
                    ),
                ),
            ),
        ),
    ),
));
ok 'bar' ~~ $constructed-nested,
    'a constructed nested lookahead lowers through the existing matcher';
