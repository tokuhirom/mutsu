use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an anonymous variable
# assigned to (`(@ = 1, 2)`) is placed over its sigil; an indented heredoc
# that starts with an interpolation starts with the empty `StrLiteral` of its
# indentation; a heredoc's range is not taken from a string of its value.

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
is placed("my \@a = (\@ = 1, 2)"),
    'VarDeclaration::Simple@0-18 Initializer::Assign@6-18 Circumfix::Parentheses@8-18 SemiList@9-17 ApplyInfix@9-17 Assignment@11-12 ArgList@9-17 VarDeclaration::Anonymous@9-10 ApplyListInfix@13-17 Infix@14-15 IntLiteral@13-14 IntLiteral@16-17',
    'an anonymous array assigned in parentheses';
is placed("my \$x = (\$ = 5)"),
    'VarDeclaration::Simple@0-15 Initializer::Assign@6-15 Circumfix::Parentheses@8-15 SemiList@9-14 ApplyInfix@9-14 Assignment@11-12 ArgList@9-14 VarDeclaration::Anonymous@9-10 IntLiteral@13-14',
    'an anonymous scalar assigned in parentheses';
is placed("my \@l = <a b>;\nsay qq:to/E/, 'z';\n    \@l[] \@l[0]\n    E\n"),
    'VarDeclaration::Simple@0-13 Initializer::Assign@6-13 QuotedString@8-13 StrLiteral@9-12 Call::Name::WithoutParentheses@15-32 Name@15-18 ArgList@19-32 Heredoc@19-27 StrLiteral@34-38 ApplyPostfix@38-42 Var::Lexical@38-40 Postcircumfix::ArrayIndex@40-42 SemiList@41-41 StrLiteral@42-43 ApplyPostfix@43-48 Var::Lexical@43-45 Postcircumfix::ArrayIndex@45-48 SemiList@46-47 IntLiteral@46-47 StrLiteral@48-49 QuotedString@29-32 StrLiteral@30-31',
    'an indented heredoc starting with an interpolation';
is placed("say q:to/E/, \"no x\\n\";\n    no x\n    E\n"),
    'Call::Name::WithoutParentheses@0-21 Name@0-3 ArgList@4-21 Heredoc@4-11 StrLiteral@23-32 QuotedString@13-21 StrLiteral@14-20',
    'a heredoc beside a string of its value';
is Q:to/X/.AST.statements[1].expression.segments.map(*.^name).join(' '),
my $x;
qq:to/E/
  $x
  E
X
    'RakuAST::StrLiteral RakuAST::Var::Lexical RakuAST::StrLiteral',
    'the indentation before a leading interpolation is a segment';
