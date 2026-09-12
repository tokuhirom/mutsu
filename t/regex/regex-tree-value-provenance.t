use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: parser-produced source trees travel with regex values
# into the ordinary smartmatch entry point. The execution plan is still the
# existing RegexPattern matcher; dynamic regex contents remain on the fallback
# path.

plan 7;

my $rx = /test/;
ok 'test' ~~ $rx, 'a parsed regex value keeps matching after storage';
nok 'toast' ~~ $rx, 'the stored regex value still rejects a non-match';
ok 'test' ~~ $rx, 'the same stored tree can be lowered again';

my $case-insensitive = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Literal.new("test"),
    adverbs => (RakuAST::ColonPair::True.new("i"),),
));
ok 'TEST' ~~ $case-insensitive, 'an adverb-bearing value uses its source tree';
nok 'toast' ~~ $case-insensitive, 'an adverb-bearing value keeps its policy';

my $constructed = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new("tree"),
    ),
);
my $constructed-value = EVAL($constructed);
ok 'tree' ~~ $constructed-value, 'a constructed tree travels through EVAL';
nok 'stale' ~~ $constructed-value, 'constructed provenance does not alter semantics';
