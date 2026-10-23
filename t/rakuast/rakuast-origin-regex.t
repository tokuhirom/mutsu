use Test;

# The nodes of a quoted regex and of a token / rule declaration carry the
# ranges Rakudo gives them (ADR-10723 Stage 2): each atom its spelling, a
# `WithWhitespace` the whitespace after its atom, a quantified atom its
# quantifier and separator, a group its brackets. Ranges measured on rakudo
# 2026.09.

plan 9;

# Each visited node of `$src` whose class name starts with `RakuAST::Regex`
# or is `QuotedRegex`, with its range.
sub regex(Str $src) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name.starts-with('Regex') || $name eq 'QuotedRegex' {
            @r.push: $name.subst('Regex::', '') ~ ' '
                ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.join(', ')
}

is regex('my $r = /abc/;'), 'QuotedRegex 8..13, Sequence 9..12, Literal 9..12',
    'a literal';
is regex('say "x" ~~ / a b+ /;'),
    'QuotedRegex 11..19, Sequence 13..18, WithWhitespace 13..15, Literal 13..14, '
    ~ 'WithWhitespace 15..18, QuantifiedAtom 15..17, Literal 15..16, Quantifier::OneOrMore 16..17',
    'a smartmatched regex: whitespace and a quantifier';
is regex('say "x" ~~ /a | bc/;'),
    'QuotedRegex 11..19, Alternation 12..18, WithWhitespace 12..14, Literal 12..13, '
    ~ 'Sequence 16..18, Literal 16..18',
    'an alternation';
is regex('my $r = / [a b]* \d . ^ $ /;'),
    'QuotedRegex 8..27, Sequence 10..26, WithWhitespace 10..17, QuantifiedAtom 10..16, '
    ~ 'Group 10..15, Sequence 11..14, WithWhitespace 11..13, Literal 11..12, Literal 13..14, '
    ~ 'Quantifier::ZeroOrMore 15..16, WithWhitespace 17..20, CharClass::Digit 17..19, '
    ~ 'WithWhitespace 20..22, CharClass::Any 20..21, WithWhitespace 22..24, '
    ~ 'Anchor::BeginningOfString 22..23, WithWhitespace 24..26, Anchor::EndOfString 24..25',
    'groups, classes and anchors';
is regex('my $r = / $<n>=(\w) <[a..z]> <-[x y]> /;'),
    'QuotedRegex 8..39, Sequence 10..38, WithWhitespace 10..20, NamedCapture 10..19, '
    ~ 'CapturingGroup 15..19, CharClass::Word 16..18, WithWhitespace 20..29, '
    ~ 'Assertion::CharClass 20..28, CharClassElement::Enumeration 21..27, '
    ~ 'CharClassEnumerationElement::Range 22..26, WithWhitespace 29..38, '
    ~ 'Assertion::CharClass 29..37, CharClassElement::Enumeration 30..36, '
    ~ 'CharClassEnumerationElement::Character 32..33, CharClassEnumerationElement::Character 34..35',
    'a named capture and character classes';
is regex('my token foo { x }; my $r = / <foo> <.foo> \. a**2 /;').split(', ')[2..*].join(', '),
    'QuotedRegex 28..52, Sequence 30..51, WithWhitespace 30..36, Assertion::Named 30..35, '
    ~ 'WithWhitespace 36..43, Assertion::Named 36..42, WithWhitespace 43..46, Literal 43..45, '
    ~ 'WithWhitespace 46..51, QuantifiedAtom 46..50, Literal 46..47, Quantifier::Range 47..50',
    'assertions, an escaped literal and a range quantifier';
is regex('my token t { a+ % "," }'),
    'WithWhitespace 13..22, QuantifiedAtom 13..22, Literal 13..14, Quantifier::OneOrMore 14..15, '
    ~ 'WithWhitespace 18..22, Quote 18..21',
    'a token with a separator';
is regex('my rule r { x y }'),
    'Sequence 12..16, WithWhitespace 12..14, Literal 12..13, WithWhitespace 14..16, Literal 14..15',
    'a rule';
is regex('say "x" ~~ m:i/ a /;'),
    'QuotedRegex 11..19, WithWhitespace 16..18, Literal 16..17',
    'an adverbed match';
