use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the declaration a leftward
# feed starts with runs up to its first arrow; a leftward feed takes the
# whole comma list on its right; a `sub` whose body declares placeholders
# has no `Signature` until it is compiled, so `visit-children` skips it and
# `.signature` is the type object.

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
is placed(Q[my @o <== map { $_ } <== 1, 2]),
    'ApplyListInfix@0-29 Feed@21-24 VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@10-20 Name@10-13 ArgList@14-20 Block@14-20 Blockoid@14-20 Var::Lexical@16-18 ApplyListInfix@25-29 Infix@26-27 IntLiteral@25-26 IntLiteral@28-29',
    'a declaration fed from the right';
is placed(Q[my @p; @p <== grep { $_ } <== 3, 4]),
    'VarDeclaration::Simple@0-5 ApplyListInfix@7-34 Feed@26-29 Var::Lexical@7-9 Call::Name::WithoutParentheses@14-25 Name@14-18 ArgList@19-25 Block@19-25 Blockoid@19-25 Var::Lexical@21-23 ApplyListInfix@30-34 Infix@31-32 IntLiteral@30-31 IntLiteral@33-34',
    'a leftward feed of a list';
is placed(Q[my &g = sub { $^a ~ "x" }]),
    'VarDeclaration::Simple@0-25 Initializer::Assign@6-25 Sub@8-25 Blockoid@12-25 ApplyInfix@14-23 Infix@18-19 ArgList@14-23 VarDeclaration::Placeholder::Positional@14-17 QuotedString@20-23 StrLiteral@21-22',
    'a placeholder sub visits no signature';
nok Q[sub { $^a }].AST.statements[0].expression.signature.defined,
    'a placeholder sub has no signature';
ok Q[sub { 1 }].AST.statements[0].expression.signature.defined,
    'a sub with no placeholders has an empty one';
