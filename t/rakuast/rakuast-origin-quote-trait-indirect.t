use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a variable followed by
# text starting with a dot in a string (`"$n.new x"`, no call without
# parentheses), `qqx` / `qx` quotes, a `does` trait on a variable and an
# indirect name under a package (`MY::(...)`).

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
is placed(Q[my $n = "x"; say "$n.new answers"]),
    'VarDeclaration::Simple@0-11 Initializer::Assign@6-11 QuotedString@8-11 StrLiteral@9-10 Call::Name::WithoutParentheses@13-33 Name@13-16 ArgList@17-33 QuotedString@17-33 Var::Lexical@18-20 StrLiteral@20-32',
    'a variable followed by a dot and text in a string';
is placed(Q[my $x = 1; say qqx/echo $x/]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 Call::Name::WithoutParentheses@11-27 Name@11-14 ArgList@15-27 QuotedString@15-27 StrLiteral@19-24 Var::Lexical@24-26',
    'an interpolating shell quote';
is placed(Q[say qx{echo 1}]),
    'Call::Name::WithoutParentheses@0-14 Name@0-3 ArgList@4-14 QuotedString@4-14 StrLiteral@7-13',
    'a non-interpolating shell quote';
is placed(Q[role S {}; my @a does S = 1, 2]),
    'Role@0-9 Name@5-6 RoleBody@5-9 Name@5-6 Blockoid@7-9 VarDeclaration::Simple@11-30 Initializer::Assign@24-30 ApplyListInfix@26-30 Infix@27-28 IntLiteral@26-27 IntLiteral@29-30 Trait::Does@17-23 Type::Simple@22-23 Name@22-23',
    'a does trait on a variable';
is placed(Q[my $x = 42; say MY::(q[$x])]),
    'VarDeclaration::Simple@0-10 Initializer::Assign@6-10 IntLiteral@8-10 Call::Name::WithoutParentheses@12-27 Name@12-15 ArgList@16-27 Term::Name@16-27 Name@16-27 QuotedString@21-26 StrLiteral@23-25',
    'an indirect name under a pseudo-package';
