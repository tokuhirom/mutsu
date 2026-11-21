use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a sigilless declaration
# (`my \x = 1`) places its `Name` after the backslash, its type before it, and
# its `Initializer` from the `=` / `:=` to the end of the value.

plan 3;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my \S = 1;]),
    'StatementList@0-10 Statement::Expression@0-9 VarDeclaration::Term@0-9 Name@4-5 Initializer::Assign@6-9 IntLiteral@8-9',
    'an assigned sigilless declaration';
is placed(Q[my \x := 2;]),
    'StatementList@0-11 Statement::Expression@0-10 VarDeclaration::Term@0-10 Name@4-5 Initializer::Bind@6-10 IntLiteral@9-10',
    'a bound sigilless declaration';
is placed(Q[my Str:D \s = "a";]),
    'StatementList@0-18 Statement::Expression@0-17 VarDeclaration::Term@0-17 Type::Definedness@3-8 Type::Simple@3-8 Name@3-6 Name@10-11 Initializer::Assign@12-17 QuotedString@14-17 StrLiteral@15-16',
    'a typed sigilless declaration';
