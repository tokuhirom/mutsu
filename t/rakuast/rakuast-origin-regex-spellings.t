use experimental :rakuast;
use Test;

# `.origin` (#15780) of regex nodes around spellings the origin walk did not
# read (#18739), measured on rakudo 2026.11: a modifier's count (`:i(0)`,
# `:1i`), a colon backtracking modifier (`*:!`, `**:!2..4`), a leading `|`,
# `||` or `&` and a branch's `&`, a positional backreference, and counts in
# any script's digits (`$٠`, `**۳`).

plan 20;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '').subst('Regex::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $n.^name.starts-with('RakuAST::Regex') }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[/ :i(0) a /]),
    'Sequence@2-10 InternalModifier::IgnoreCase@2-7 WithWhitespace@8-10 Literal@8-9',
    '/ :i(0) a /';
is placed(Q[/ :1i a /]),
    'Sequence@2-8 InternalModifier::IgnoreCase@2-5 WithWhitespace@6-8 Literal@6-7',
    '/ :1i a /';
is placed(Q[/ xa*:!y /]),
    'Sequence@2-9 Literal@2-3 QuantifiedAtom@3-7 Literal@3-4 Quantifier::ZeroOrMore@4-7 WithWhitespace@7-9 Literal@7-8',
    '/ xa*:!y /';
is placed(Q[/ a |& b /]),
    'Alternation@2-9 WithWhitespace@2-4 Literal@2-3 WithWhitespace@7-9 Literal@7-8',
    '/ a |& b /';
is placed(Q[/ & a /]),
    'WithWhitespace@4-6 Literal@4-5',
    '/ & a /';
is placed(Q[/ (.) $٠ /]),
    'Sequence@2-9 WithWhitespace@2-6 CapturingGroup@2-5 CharClass::Any@3-4 WithWhitespace@6-9 BackReference::Positional@6-8',
    '/ (.) $٠ /';
is placed(Q[/ a**۳ /]),
    'WithWhitespace@2-7 QuantifiedAtom@2-6 Literal@2-3 Quantifier::Range@3-6',
    '/ a**۳ /';
is placed(Q[/ a**:!2..4 /]),
    'WithWhitespace@2-12 QuantifiedAtom@2-11 Literal@2-3 Quantifier::Range@3-11',
    '/ a**:!2..4 /';
is placed(Q[/ :!i(1) a /]),
    'Sequence@2-11 InternalModifier::IgnoreCase@2-5 WithWhitespace@5-9 CapturingGroup@5-8 Literal@6-7 WithWhitespace@9-11 Literal@9-10',
    '/ :!i(1) a /';
is placed(Q[/ x [ | a | b ] /]),
    'Sequence@2-16 WithWhitespace@2-4 Literal@2-3 WithWhitespace@4-16 Group@4-15 Alternation@6-14 WithWhitespace@8-10 Literal@8-9 WithWhitespace@12-14 Literal@12-13',
    '/ x [ | a | b ] /';
is placed(Q[/ || a /]),
    'WithWhitespace@5-7 Literal@5-6',
    '/ || a /';
is placed(Q[/ a | & b /]),
    'Alternation@2-10 WithWhitespace@2-4 Literal@2-3 WithWhitespace@8-10 Literal@8-9',
    '/ a | & b /';
is placed(Q[/ a*? b /]),
    'Sequence@2-8 WithWhitespace@2-6 QuantifiedAtom@2-5 Literal@2-3 Quantifier::ZeroOrMore@3-5 WithWhitespace@6-8 Literal@6-7',
    '/ a*? b /';
is placed(Q[/ a*:? b /]),
    'Sequence@2-9 WithWhitespace@2-7 QuantifiedAtom@2-6 Literal@2-3 Quantifier::ZeroOrMore@3-6 WithWhitespace@7-9 Literal@7-8',
    '/ a*:? b /';
is placed(Q[/ :!i(0) a /]),
    'Sequence@2-11 InternalModifier::IgnoreCase@2-5 WithWhitespace@5-9 CapturingGroup@5-8 Literal@6-7 WithWhitespace@9-11 Literal@9-10',
    '/ :!i(0) a /';
is placed(Q[/ :i(1) a /]),
    'Sequence@2-10 InternalModifier::IgnoreCase@2-7 WithWhitespace@8-10 Literal@8-9',
    '/ :i(1) a /';
is placed(Q[/ :2s a /]),
    'Sequence@2-8 InternalModifier::Sigspace@2-5 WithWhitespace@6-8 Literal@6-7',
    '/ :2s a /';
is placed(Q[/ (a)(b) $1 /]),
    'Sequence@2-12 CapturingGroup@2-5 Literal@3-4 WithWhitespace@5-9 CapturingGroup@5-8 Literal@6-7 WithWhitespace@9-12 BackReference::Positional@9-11',
    '/ (a)(b) $1 /';
is placed(Q[/ a**٢..* /]),
    'WithWhitespace@2-10 QuantifiedAtom@2-9 Literal@2-3 Quantifier::Range@3-9',
    '/ a**٢..* /';
is placed(Q[/ a+: b /]),
    'Sequence@2-8 WithWhitespace@2-6 QuantifiedAtom@2-5 Literal@2-3 Quantifier::OneOrMore@3-5 WithWhitespace@6-8 Literal@6-7',
    '/ a+: b /';
