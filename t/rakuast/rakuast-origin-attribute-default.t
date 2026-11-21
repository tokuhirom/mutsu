use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an attribute default has
# an origin, and a `Trait::WillBuild` visited beside it, only when it is a
# literal value or a name; any other default is visited once, inside its
# unplaced thunk. A public attribute interpolated in a string is placed.

plan 15;
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
is placed(Q[class C0 { has $.a = 1 }]),
    'Class@0-24 Name@6-8 Block@9-24 Blockoid@9-24 VarDeclaration::Simple@11-22 Initializer::Assign@19-22 IntLiteral@21-22 IntLiteral@21-22',
    'an integer default';
is placed(Q[class C1 { has $.a = "x" }]),
    'Class@0-26 Name@6-8 Block@9-26 Blockoid@9-26 VarDeclaration::Simple@11-24 Initializer::Assign@19-24 QuotedString@21-24 StrLiteral@22-23 QuotedString@21-24 StrLiteral@22-23',
    'a string default';
is placed(Q[class C2 { has $.a = 1 + 2 }]),
    'Class@0-28 Name@6-8 Block@9-28 Blockoid@9-28 VarDeclaration::Simple@11-26 ApplyInfix@21-26 Infix@23-24 ArgList@21-26 IntLiteral@21-22 IntLiteral@25-26',
    'an infix default';
is placed(Q[class C3 { has $.a = [1] }]),
    'Class@0-26 Name@6-8 Block@9-26 Blockoid@9-26 VarDeclaration::Simple@11-24 Circumfix::ArrayComposer@21-24 SemiList@22-23 IntLiteral@22-23',
    'an array composer default';
is placed(Q[class C4 { has $.a = -1 }]),
    'Class@0-25 Name@6-8 Block@9-25 Blockoid@9-25 VarDeclaration::Simple@11-23 ApplyPrefix@21-23 Prefix@21-22 IntLiteral@22-23',
    'a negated default';
is placed(Q[class C5 { has $.a = 1.5 }]),
    'Class@0-26 Name@6-8 Block@9-26 Blockoid@9-26 VarDeclaration::Simple@11-24 Initializer::Assign@19-24 RatLiteral@21-24 RatLiteral@21-24',
    'a rational default';
is placed(Q[class C6 { has $.a = True }]),
    'Class@0-27 Name@6-8 Block@9-27 Blockoid@9-27 VarDeclaration::Simple@11-25 Initializer::Assign@19-25 Term::Enum@21-25 Name@21-25 Term::Enum@21-25 Name@21-25',
    'a True default';
is placed(Q[class C7 { has $.a = <a> }]),
    'Class@0-26 Name@6-8 Block@9-26 Blockoid@9-26 VarDeclaration::Simple@11-24 Initializer::Assign@19-24 QuotedString@21-24 StrLiteral@22-23 QuotedString@21-24 StrLiteral@22-23',
    'a word-quote default';
is placed(Q[class C8 { has $.a = Int }]),
    'Class@0-26 Name@6-8 Block@9-26 Blockoid@9-26 VarDeclaration::Simple@11-24 Type::Simple@21-24 Name@21-24',
    'a type default';
is placed(Q[class C9 { has $.a = pi }]),
    'Class@0-25 Name@6-8 Block@9-25 Blockoid@9-25 VarDeclaration::Simple@11-23 Initializer::Assign@19-23 Term::Name@21-23 Name@21-23 Term::Name@21-23 Name@21-23',
    'a named-term default';
is placed(Q[class C10 { has $.a = :b(1) }]),
    'Class@0-29 Name@6-9 Block@10-29 Blockoid@10-29 VarDeclaration::Simple@12-27 Initializer::Assign@20-27 ColonPair::Value@22-27 Circumfix::Parentheses@24-27 SemiList@25-26 IntLiteral@25-26 ColonPair::Value@22-27 Circumfix::Parentheses@24-27 SemiList@25-26 IntLiteral@25-26',
    'a colonpair default';
is placed(Q[class C11 { has $.a = a => 1 }]),
    'Class@0-30 Name@6-9 Block@10-30 Blockoid@10-30 VarDeclaration::Simple@12-28 FatArrow@22-28 IntLiteral@27-28',
    'a pair default';
is placed(Q[class C12 { has $.a = { "g" } }]),
    'Class@0-31 Name@6-9 Block@10-31 Blockoid@10-31 VarDeclaration::Simple@12-29 Block@22-29 Blockoid@22-29 QuotedString@24-27 StrLiteral@25-26',
    'a block default';
is placed(Q[class P1 { has $.n = 3; method m { "n=$.n" } }]),
    'Class@0-46 Name@6-8 Block@9-46 Blockoid@9-46 VarDeclaration::Simple@11-22 Initializer::Assign@19-22 IntLiteral@21-22 IntLiteral@21-22 Method@24-44 Name@31-32 Blockoid@33-44 QuotedString@35-42 StrLiteral@36-38 Var::Attribute::Public@38-41',
    'a public scalar attribute in a string';
is placed(Q[class P2 { has @.a; method m { "a=@.a[]" } }]),
    'Class@0-44 Name@6-8 Block@9-44 Blockoid@9-44 VarDeclaration::Simple@11-18 Method@20-42 Name@27-28 Blockoid@29-42 QuotedString@31-40 StrLiteral@32-34 ApplyPostfix@34-39 Var::Attribute::Public@34-37 Postcircumfix::ArrayIndex@37-39 SemiList@38-38',
    'a public array attribute in a string';
