use Test;

# A regex literal's outer adverbs stay on the QuotedRegex node, around the
# plain body, as measured on rakudo 2026.09.

plan 10;

sub quoted($src) { $src.AST.statements.head.expression }

my $rx = quoted(Q|rx:i/index/|);
is $rx.adverbs.elems, 1, 'rx:i has one adverb';
isa-ok $rx.adverbs[0], RakuAST::ColonPair::True, 'a ColonPair::True';
is $rx.adverbs[0].key, 'i', 'named i';
isa-ok $rx.body, RakuAST::Regex::Sequence, 'the body has no InternalModifier';
isa-ok $rx.body.terms[0], RakuAST::Regex::Literal, 'only the literal';

is quoted(Q|rx:m:ratchet/x/|).adverbs.map(*.key).join(','), 'm,ratchet',
    'every modifier adverb is kept, in source order';
is quoted(Q|m:s:g/a b/|).adverbs.map(*.key).join(','), 's,g',
    'm// keeps a modifier beside a match-time adverb';

ok "INDEX" ~~ EVAL(Q|rx:i/index/|.AST), 'rx:i still ignores case after the round trip';
ok "ä" ~~ EVAL(Q|rx:m/a/|.AST), 'rx:m still ignores marks';
nok "ab" ~~ EVAL(Q|rx:s/a b/|.AST), 'rx:s still makes whitespace significant';
