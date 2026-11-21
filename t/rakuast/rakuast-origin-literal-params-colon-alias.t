use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: in a routine literal passed
# as an argument, a parameter read again as an infix's left operand
# (`$d + 1`) is placed right before the operator; an aliased named assertion
# with colon arguments (`<alias=word: "a">`) is placed.

plan 3;
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
is placed(Q[sub on(&f) { 1 }; say on(sub ($d) { $d + 1; $d })]),
    'Sub@0-16 Name@4-6 Signature@7-9 Parameter@7-9 ParameterTarget::Var@7-9 VarDeclaration::Simple@7-9 Blockoid@11-16 IntLiteral@13-14 Call::Name::WithoutParentheses@18-49 Name@18-21 ArgList@22-49 Call::Name@22-49 Name@22-24 ArgList@25-48 Sub@25-48 Signature@30-32 Parameter@30-32 ParameterTarget::Var@30-32 VarDeclaration::Simple@30-32 Blockoid@34-48 ApplyInfix@36-42 Infix@39-40 ArgList@36-42 Var::Lexical@36-38 IntLiteral@41-42 Var::Lexical@44-46',
    'a parameter read again in an infix';
is placed(Q[sub on(&f) { 1 }; say on(sub ($c is copy) { $c ~~ s:g/\d+/#/; $c })]),
    'Sub@0-16 Name@4-6 Signature@7-9 Parameter@7-9 ParameterTarget::Var@7-9 VarDeclaration::Simple@7-9 Blockoid@11-16 IntLiteral@13-14 Call::Name::WithoutParentheses@18-67 Name@18-21 ArgList@22-67 Call::Name@22-67 Name@22-24 ArgList@25-66 Sub@25-66 Signature@30-40 Parameter@30-40 ParameterTarget::Var@30-32 VarDeclaration::Simple@30-32 Trait::Is@33-40 Name@36-40 Blockoid@42-66 ApplyInfix@44-60 Infix@47-49 ArgList@44-60 Var::Lexical@44-46 Substitution@50-60 ColonPair::True@51-53 Regex::QuantifiedAtom@54-57 Regex::CharClass::Digit@54-56 Regex::Quantifier::OneOrMore@56-57 QuotedString@58-59 StrLiteral@58-59 Var::Lexical@62-64',
    'a parameter read again in a smartmatch';
is placed(Q[grammar G { token TOP { <alias=word: "a"> }; token word($x) { \w } }]),
    'Grammar@0-68 Name@8-9 Block@10-68 Blockoid@10-68 TokenDeclaration@12-43 Name@18-21 Regex::WithWhitespace@24-42 Regex::Assertion::Alias@24-41 Regex::Assertion::Named::Args@31-40 Name@31-35 ArgList@37-40 QuotedString@37-40 StrLiteral@38-39 TokenDeclaration@45-66 Name@51-55 Signature@56-58 Parameter@56-58 ParameterTarget::Var@56-58 VarDeclaration::Simple@56-58 Regex::WithWhitespace@62-65 Regex::CharClass::Word@62-64',
    'an aliased colon-argument assertion';
