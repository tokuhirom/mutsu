use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a character enumeration
# places its literal `+`, `-` and `.` and its backslash-escaped characters
# (`<[+-]>`, `<[\-.]>`), and an anonymous `token { ... }` / `rule { ... }`
# places its body as a named one does.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Regex|Token|Rule/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[/<[+-]> \d+/]),
    'QuotedRegex@0-12 Regex::Sequence@1-11 Regex::WithWhitespace@1-8 Regex::Assertion::CharClass@1-7 Regex::CharClassElement::Enumeration@2-6 Regex::CharClassEnumerationElement::Character@3-4 Regex::CharClassEnumerationElement::Character@4-5 Regex::QuantifiedAtom@8-11 Regex::CharClass::Digit@8-10 Regex::Quantifier::OneOrMore@10-11',
    'plus and minus in an enumeration';
is placed(Q[/<[\-.]>/]),
    'QuotedRegex@0-9 Regex::Assertion::CharClass@1-8 Regex::CharClassElement::Enumeration@2-7 Regex::CharClassEnumerationElement::Character@3-5 Regex::CharClassEnumerationElement::Character@5-6',
    'an escaped minus and a dot';
is placed(Q[my $t = token { ab };]),
    'TokenDeclaration@8-20 Regex::Sequence@16-19 Regex::WithWhitespace@16-19 Regex::Literal@16-18',
    'an anonymous token';
is placed(Q[my $r = rule { x };]),
    'RuleDeclaration@8-18 Regex::WithWhitespace@15-17 Regex::Literal@15-16',
    'an anonymous rule';
