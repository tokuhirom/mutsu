use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a `CATCH` / `CONTROL`
# block is placed after its keyword; an anonymous declaration places its
# initializer; a statement with a modifier inside parentheses places its
# expression and itself.

plan 4;
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
is placed(Q[{ die "x"; CATCH { default { say .message } } }]),
    'Block@0-47 Blockoid@0-47 Call::Name::WithoutParentheses@2-9 Name@2-5 ArgList@6-9 QuotedString@6-9 StrLiteral@7-8 Block@17-45 Blockoid@17-45 Block@27-43 Blockoid@27-43 Call::Name::WithoutParentheses@29-41 Name@29-32 ArgList@33-41 Term::TopicCall@33-41 Call::Method@34-41 Name@34-41 ArgList@41-41',
    'a CATCH block';
is placed(Q[my \bar = my $ = -3]),
    'VarDeclaration::Term@0-19 Name@4-7 Initializer::Assign@8-19 VarDeclaration::Anonymous@10-19 Initializer::Assign@15-19 ApplyPrefix@17-19 Prefix@17-18 IntLiteral@18-19',
    'an anonymous declaration with an initializer';
is placed(Q[my $z = (1 if True)]),
    'VarDeclaration::Simple@0-19 Initializer::Assign@6-19 Circumfix::Parentheses@8-19 SemiList@9-18 IntLiteral@9-10 Term::Enum@14-18 Name@14-18',
    'a statement with a modifier in parentheses';
is placed(Q[my $c = sub ($v) { $v }; my $y = ($c(1) if True)]),
    'VarDeclaration::Simple@0-23 Initializer::Assign@6-23 Sub@8-23 Signature@13-15 Parameter@13-15 ParameterTarget::Var@13-15 VarDeclaration::Simple@13-15 Blockoid@17-23 Var::Lexical@19-21 VarDeclaration::Simple@25-48 Initializer::Assign@31-48 Circumfix::Parentheses@33-48 SemiList@34-47 ApplyPostfix@34-39 Var::Lexical@34-36 Call::Term@36-39 ArgList@37-38 IntLiteral@37-38 Term::Enum@43-47 Name@43-47',
    'a call with a modifier in parentheses';
