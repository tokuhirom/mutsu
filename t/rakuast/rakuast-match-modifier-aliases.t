use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# The combined :ii/:ss modifiers remain on the QuotedRegex as written and
# continue through the existing match execution path.

plan 10;

sub quoted($src) { $src.AST.statements.head.expression }

my $samecase = quoted(Q|m:ii/hello/|);
isa-ok $samecase, RakuAST::QuotedRegex, ':ii is a QuotedRegex';
ok $samecase.match-immediately, ':ii matches immediately';
is $samecase.adverbs.map(*.key).join(','), 'ii', ':ii keeps its written adverb';
ok 'Hello' ~~ EVAL($samecase), ':ii still matches case-insensitively';

my $samespace = quoted(Q|m:ss/a b/|);
isa-ok $samespace, RakuAST::QuotedRegex, ':ss is a QuotedRegex';
ok $samespace.match-immediately, ':ss matches immediately';
is $samespace.adverbs.map(*.key).join(','), 'ss', ':ss keeps its written adverb';
ok 'a  b' ~~ EVAL($samespace), ':ss still makes whitespace flexible';
nok 'ab' ~~ EVAL($samespace), ':ss still requires whitespace';

is EVAL(Q|"Hello hello" ~~ m:g:ii/hello/; $/.elems|.AST), 2,
    'combined match-time and :ii modifiers keep their execution behavior';
