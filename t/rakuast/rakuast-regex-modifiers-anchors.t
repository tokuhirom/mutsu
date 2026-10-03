use Test;

# The :s / :r internal modifiers and the word-boundary anchors in RakuAST,
# measured on rakudo 2026.09.

plan 12;

sub body($src) { $src.AST.statements.head.expression.body }

my @terms = body(Q|/:s a :!r b :sigspace c :ratchet d/|).terms;
isa-ok @terms[0], RakuAST::Regex::InternalModifier::Sigspace, ':s is Sigspace';
is @terms[0].modifier, 's', 'with the short spelling';
isa-ok @terms[2], RakuAST::Regex::InternalModifier::Ratchet, ':!r is Ratchet';
is-deeply @terms[2].negated, True, 'negated';
is @terms[4].modifier, 'sigspace', 'the long spelling is kept';
isa-ok @terms[6], RakuAST::Regex::InternalModifier::Ratchet, ':ratchet is Ratchet';

my @anchors = body(Q|/« a » <<b>>/|).terms;
isa-ok @anchors[0].regex, RakuAST::Regex::Anchor::LeftWordBoundary, '« is LeftWordBoundary';
isa-ok @anchors[2].regex, RakuAST::Regex::Anchor::RightWordBoundary, '» is RightWordBoundary';
isa-ok @anchors[3], RakuAST::Regex::Anchor::LeftWordBoundary, '<< is LeftWordBoundary';
isa-ok @anchors[3], RakuAST::Regex::Anchor, 'which is an Anchor';

ok "foo bar" ~~ EVAL(Q|/:s foo bar/|.AST), ':s makes whitespace significant';
nok "afoob" ~~ EVAL(Q|/« foo »/|.AST), 'word boundaries survive the round trip';
