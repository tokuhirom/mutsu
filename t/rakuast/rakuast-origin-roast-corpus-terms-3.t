use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for a third set of
# constructs the roast corpus showed unplaced. A stub (`...`, `!!!`) also
# visits its `ArgList`, as Rakudo's does.

plan 14;
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
is placed(Q[say Q«« a »»]),
    'Call::Name::WithoutParentheses@0-12 Name@0-3 ArgList@4-12 QuotedString@4-12 StrLiteral@7-10',
    'a doubled guillemet quote';
is placed(Q[say "a" ~~ /<[a] +[b] -:Digit>/]),
    'Call::Name::WithoutParentheses@0-31 Name@0-3 ArgList@4-31 ApplyInfix@4-31 Infix@8-10 ArgList@4-31 QuotedString@4-7 StrLiteral@5-6 QuotedRegex@11-31 Regex::Assertion::CharClass@12-30 Regex::CharClassElement::Enumeration@13-17 Regex::CharClassEnumerationElement::Character@14-15 Regex::CharClassElement::Enumeration@17-22 Regex::CharClassEnumerationElement::Character@19-20 Regex::CharClassElement::Property@22-29',
    'regex property and enumeration elements';
is placed(Q[my Int:_ $a = 1;]),
    'VarDeclaration::Simple@0-15 Type::AnyDefinedness@3-8 Type::Simple@3-8 Name@3-6 Initializer::Assign@12-15 IntLiteral@14-15',
    'an any-definedness declaration type';
is placed(Q[my $l = (−∞…∞);]),
    'VarDeclaration::Simple@0-14 Initializer::Assign@6-14 Circumfix::Parentheses@8-14 SemiList@9-13 ApplyListInfix@9-13 Infix@11-12 ApplyPrefix@9-11 Prefix@9-10 NumLiteral@10-11 NumLiteral@12-13',
    'a U+2212 prefix minus';
is placed(Q[class RT { }; say RT::.WHAT]),
    'Class@0-12 Name@6-8 Block@9-12 Blockoid@9-12 Call::Name::WithoutParentheses@14-27 Name@14-17 ArgList@18-27 ApplyPostfix@18-27 Term::Name@18-22 Name@18-22 Call::Method@23-27 Name@23-27 ArgList@27-27',
    'a package name ending in ::';
is placed(Q[my %what; say "foo3" ~~ /$<cat>=@(%what.keys) 4/]),
    'VarDeclaration::Simple@0-8 Call::Name::WithoutParentheses@10-48 Name@10-13 ArgList@14-48 ApplyInfix@14-48 Infix@21-23 ArgList@14-48 QuotedString@14-20 StrLiteral@15-19 QuotedRegex@24-48 Regex::Sequence@25-47 Regex::WithWhitespace@25-46 Regex::NamedCapture@25-45 Regex::Interpolation@32-45 Contextualizer::List@32-45 ApplyPostfix@34-44 Var::Lexical@34-39 Call::Method@40-44 Name@40-44 ArgList@44-44 Regex::Literal@46-47',
    'an interpolated regex contextualizer';
is placed(Q[class A { has @.b is Array[Int]; }]),
    'Class@0-34 Name@6-7 Block@8-34 Blockoid@8-34 VarDeclaration::Simple@10-31 Trait::Is@18-31 Type::Parameterized@21-31 Type::Simple@21-31 Name@21-26 ArgList@27-30 Type::Simple@27-30 Name@27-30',
    'a parameterized is trait on an attribute';
is placed(Q[module Foo { our sub run { 1 } }; say Foo::run().chars]),
    'Module@0-32 Name@7-10 Block@11-32 Blockoid@11-32 Sub@13-30 Name@21-24 Blockoid@25-30 IntLiteral@27-28 Call::Name::WithoutParentheses@34-54 Name@34-37 ArgList@38-54 ApplyPostfix@38-54 Call::Name@38-48 Name@38-46 ArgList@47-47 Call::Method@49-54 Name@49-54 ArgList@54-54',
    'a qualified call with parentheses';
is placed(Q[my $str; my $retval = INIT { $str ~= q[I] };]),
    'VarDeclaration::Simple@0-7 VarDeclaration::Simple@9-43 Initializer::Assign@20-43 Block@27-43 Blockoid@27-43 ApplyInfix@29-41 MetaInfix::Assign@34-36 Infix@34-35 ArgList@29-41 Var::Lexical@29-33 QuotedString@37-41 StrLiteral@39-40',
    'a phaser block in an initializer';
is placed(Q[temp $_ = 42;]),
    'ApplyInfix@0-12 Assignment@8-9 ArgList@0-12 ApplyPrefix@0-7 Prefix@0-4 Var::Lexical@5-7 IntLiteral@10-12',
    'a temp prefix assigned to';
is placed(Q[my @md[2;*];]),
    'VarDeclaration::Simple@0-11 SemiList@7-10 IntLiteral@7-8 Term::Whatever@9-10',
    'literal and Whatever shape dimensions';
is placed(Q[for 1 { NEXT $++; }]),
    'IntLiteral@4-5 Block@6-19 Blockoid@6-19 ApplyPostfix@13-16 VarDeclaration::Anonymous@13-14 Postfix@14-16',
    'an anonymous variable under a phaser';
is placed(Q[my proto sub a($, $?) { * }]),
    'Sub@0-27 Name@13-14 Signature@15-20 Parameter@15-16 ParameterTarget::Var@15-16 VarDeclaration::Anonymous@15-16 Parameter@18-20 ParameterTarget::Var@18-19 VarDeclaration::Anonymous@18-19 OnlyStar@22-27',
    'anonymous parameters, one optional';
is placed(Q[sub g { !!! "x" }]),
    'Sub@0-17 Name@4-5 Blockoid@6-17 Stub::Die@8-15 ArgList@12-15 QuotedString@12-15 StrLiteral@13-14',
    'a stub with a message';
