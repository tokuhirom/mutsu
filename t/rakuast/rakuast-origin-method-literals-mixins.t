use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a method literal's body is
# placed as a sub literal's, and both find their braces after a signature; a
# statement list ending in a variable places it; a sigilless term assigned to
# (`foo = 7`), a dotted subscript in a string (`"@x.[1]"`) and the `but`
# mixin operator are placed.

plan 6;
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
is placed(Q[my $m = method { 1 }]),
    'VarDeclaration::Simple@0-20 Initializer::Assign@6-20 Method@8-20 Blockoid@15-20 IntLiteral@17-18',
    'a method literal';
is placed(Q[my $s = sub ($a) { $a }]),
    'VarDeclaration::Simple@0-23 Initializer::Assign@6-23 Sub@8-23 Signature@13-15 Parameter@13-15 ParameterTarget::Var@13-15 VarDeclaration::Simple@13-15 Blockoid@17-23 Var::Lexical@19-21',
    'a sub literal with a signature';
is placed(Q[my $k = method ($c) { 1; $c }]),
    'VarDeclaration::Simple@0-29 Initializer::Assign@6-29 Method@8-29 Signature@16-18 Parameter@16-18 ParameterTarget::Var@16-18 VarDeclaration::Simple@16-18 Blockoid@20-29 IntLiteral@22-23 Var::Lexical@25-27',
    'a method literal with a signature ending in its parameter';
is placed(Q[my \foo = 3; foo = 7]),
    'VarDeclaration::Term@0-11 Name@4-7 Initializer::Assign@8-11 IntLiteral@10-11 ApplyInfix@13-20 Assignment@17-18 ArgList@13-20 Term::Name@13-16 Name@13-16 IntLiteral@19-20',
    'a sigilless term assigned to';
is placed(Q[my @x = 1, 2; say "@x.[1]"]),
    'VarDeclaration::Simple@0-12 Initializer::Assign@6-12 ApplyListInfix@8-12 Infix@9-10 IntLiteral@8-9 IntLiteral@11-12 Call::Name::WithoutParentheses@14-26 Name@14-17 ArgList@18-26 QuotedString@18-26 ApplyPostfix@19-25 Var::Lexical@19-21 Postcircumfix::ArrayIndex@22-25 SemiList@23-24 IntLiteral@23-24',
    'a dotted subscript interpolated in a string';
is placed(Q[role Tag {}; my $t = 1 but Tag]),
    'Role@0-11 Name@5-8 RoleBody@5-11 Name@5-8 Blockoid@9-11 VarDeclaration::Simple@13-30 Initializer::Assign@19-30 ApplyInfix@21-30 Mixin@23-26 ArgList@21-30 IntLiteral@21-22 Type::Simple@27-30 Name@27-30',
    'a mixin';
