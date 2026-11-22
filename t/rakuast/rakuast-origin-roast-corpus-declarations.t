use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for declarations and
# regex forms the roast corpus showed unplaced. A role's signature also
# visits the implicit parameter capturing the class it is composed into.

plan 9;
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
is placed(Q[role P[$x, *@t] { }]),
    'Role@0-19 Name@5-6 RoleBody@5-19 Name@5-6 Signature@7-14 Parameter@7-9 ParameterTarget::Var@7-9 VarDeclaration::Simple@7-9 Parameter@11-14 ParameterTarget::Var@12-14 VarDeclaration::Simple@12-14 Blockoid@16-19',
    'a role parameterization with a slurpy';
is placed(Q[my &k = -> *@a { }]),
    'VarDeclaration::Simple@0-18 Initializer::Assign@6-18 PointyBlock@8-18 Signature@11-14 Parameter@11-14 ParameterTarget::Var@12-14 VarDeclaration::Simple@12-14 Blockoid@15-18',
    'a slurpy pointy parameter';
is placed(Q[say "abcd" ~~ m/a $1=(.) $0=(.) d/]),
    'Call::Name::WithoutParentheses@0-34 Name@0-3 ArgList@4-34 ApplyInfix@4-34 Infix@11-13 ArgList@4-34 QuotedString@4-10 StrLiteral@5-9 QuotedRegex@14-34 Regex::Sequence@16-33 Regex::WithWhitespace@16-18 Regex::Literal@16-17 Regex::WithWhitespace@18-25 Regex::NamedCapture@18-24 Regex::CapturingGroup@21-24 Regex::CharClass::Any@22-23 Regex::WithWhitespace@25-32 Regex::NamedCapture@25-31 Regex::CapturingGroup@28-31 Regex::CharClass::Any@29-30 Regex::Literal@32-33',
    'numbered capture aliases';
is placed(Q[role R[::T] { }; class C does R[Int:D] {}]),
    'Role@0-15 Name@5-6 RoleBody@5-15 Name@5-6 Signature@7-10 Parameter@7-10 Blockoid@12-15 Class@17-41 Name@23-24 Block@25-41 Trait::Does@25-38 Type::Parameterized@30-38 Type::Simple@30-38 Name@30-31 ArgList@32-37 Type::Definedness@32-37 Type::Simple@32-37 Name@32-35 Blockoid@39-41',
    'a parameterized does trait and a role type capture';
is placed(Q[my %ex = %(:cat2{:sub2[2, 3]})]),
    'VarDeclaration::Simple@0-30 Initializer::Assign@7-30 Contextualizer::Hash@9-30 ColonPair::Value@11-29 Circumfix::HashComposer@16-29 ColonPair::Value@17-28 Circumfix::ArrayComposer@22-28 SemiList@23-27 ApplyListInfix@23-27 Infix@24-25 IntLiteral@23-24 IntLiteral@26-27',
    'a colonpair in a nested hash composer';
is placed(Q[my &as = { "val $^a" }]),
    'VarDeclaration::Simple@0-22 Initializer::Assign@7-22 Block@9-22 Blockoid@9-22 QuotedString@11-20 StrLiteral@12-16 VarDeclaration::Placeholder::Positional@16-19',
    'a placeholder interpolated in a string';
is placed(Q[say "a cdef" ~~ m/<after a <.ws> c> def/]),
    'Call::Name::WithoutParentheses@0-40 Name@0-3 ArgList@4-40 ApplyInfix@4-40 Infix@13-15 ArgList@4-40 QuotedString@4-12 StrLiteral@5-11 QuotedRegex@16-40 Regex::Sequence@18-39 Regex::WithWhitespace@18-36 Regex::Assertion::Named::RegexArg@18-35 Name@19-24 Regex::Sequence@25-34 Regex::WithWhitespace@25-27 Regex::Literal@25-26 Regex::WithWhitespace@27-33 Regex::Assertion::Named@27-32 Name@29-31 Regex::Literal@33-34 Regex::Literal@36-39',
    'a standalone regex-argument assertion';
is placed(Q[my $j = 1; MY::<$j> = 111;]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 ApplyPostfix@11-25 Term::Name@11-15 Name@11-15 Postcircumfix::LiteralHashIndex@15-25 QuotedString@16-18 StrLiteral@16-18 IntLiteral@22-25',
    'a package-name subscript assigned to';
{
    my @seen;
    sub walk($n, $d = 0) { @seen.push(('  ' x $d) ~ $n.^name); $n.visit-children({ walk($_, $d + 1) }) }
    walk(Q[role P { }].AST.statements[0].expression);
    is @seen[5], '      RakuAST::Parameter', 'a role signature visits its implicit class parameter';
}
