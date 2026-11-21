use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an aliased named assertion
# with arguments (`<alias=word("a")>`), a regex literal argument
# (`<v(/\d+/)>`) and a character written as an escape in a class
# (`<-[\x[22]]>`, `<[\x[41]..\x[5A]]>`) are placed.

plan 5;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[grammar G { token TOP { <alias=word("a")> }; token word($x) { \w } }]),
    'Grammar@0-68 Name@8-9 Block@10-68 Blockoid@10-68 TokenDeclaration@12-43 Name@18-21 Regex::WithWhitespace@24-42 Regex::Assertion::Alias@24-41 Regex::Assertion::Named::Args@31-40 Name@31-35 ArgList@36-39 QuotedString@36-39 StrLiteral@37-38 TokenDeclaration@45-66 Name@51-55 Signature@56-58 Parameter@56-58 ParameterTarget::Var@56-58 VarDeclaration::Simple@56-58 Regex::WithWhitespace@62-65 Regex::CharClass::Word@62-64',
    'an alias of a named assertion with arguments';
is placed(Q[grammar H { token v($r) { <$r> }; token T { <v(/\d+/)> } }]),
    'Grammar@0-58 Name@8-9 Block@10-58 Blockoid@10-58 TokenDeclaration@12-32 Name@18-19 Signature@20-22 Parameter@20-22 ParameterTarget::Var@20-22 VarDeclaration::Simple@20-22 Regex::WithWhitespace@26-31 Regex::Assertion::InterpolatedVar@26-30 Var::Lexical@27-29 TokenDeclaration@34-56 Name@40-41 Regex::WithWhitespace@44-55 Regex::Assertion::Named::Args@44-54 Name@45-46 ArgList@47-52 QuotedRegex@47-52 Regex::QuantifiedAtom@48-51 Regex::CharClass::Digit@48-50 Regex::Quantifier::OneOrMore@50-51',
    'a regex literal argument';
is placed(Q[say "a" ~~ /<-[\x[22]]>/]),
    'Call::Name::WithoutParentheses@0-24 Name@0-3 ArgList@4-24 ApplyInfix@4-24 Infix@8-10 ArgList@4-24 QuotedString@4-7 StrLiteral@5-6 QuotedRegex@11-24 Regex::Assertion::CharClass@12-23 Regex::CharClassElement::Enumeration@13-22 Regex::CharClass::Specified@15-21',
    'an escaped character in a class';
is placed(Q[say "a" ~~ /<[\x[41]..\x[5A]]>/]),
    'Call::Name::WithoutParentheses@0-31 Name@0-3 ArgList@4-31 ApplyInfix@4-31 Infix@8-10 ArgList@4-31 QuotedString@4-7 StrLiteral@5-6 QuotedRegex@11-31 Regex::Assertion::CharClass@12-30 Regex::CharClassElement::Enumeration@13-29 Regex::CharClassEnumerationElement::Range@14-28',
    'escaped range bounds';
is placed(Q[say "a" ~~ /<?before <-[\x[22]]>>/]),
    'Call::Name::WithoutParentheses@0-34 Name@0-3 ArgList@4-34 ApplyInfix@4-34 Infix@8-10 ArgList@4-34 QuotedString@4-7 StrLiteral@5-6 QuotedRegex@11-34 Regex::Assertion::Lookahead@12-33 Regex::Assertion::Named::RegexArg@14-32 Name@14-20 Regex::Assertion::CharClass@21-32 Regex::CharClassElement::Enumeration@22-31 Regex::CharClass::Specified@24-30',
    'an escaped class inside a lookahead';
