use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a listop call in the body
# of an anonymous sub (`sub { say 1 }`), whose statements the parser marks
# with no line, runs from its name to the end of its last argument.

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
is placed(Q[sub { say 1 }]),
    'StatementList@0-13 Statement::Expression@0-13 Sub@0-13 Blockoid@4-13 StatementList@6-11 Statement::Expression@6-11 Call::Name::WithoutParentheses@6-11 Name@6-9 ArgList@10-11 IntLiteral@10-11',
    'say in an anonymous sub';
is placed(Q[my $t = sub { say 1; 2 }]),
    'StatementList@0-24 Statement::Expression@0-24 VarDeclaration::Simple@0-24 Initializer::Assign@6-24 Sub@8-24 Blockoid@12-24 StatementList@14-22 Statement::Expression@14-19 Call::Name::WithoutParentheses@14-19 Name@14-17 ArgList@18-19 IntLiteral@18-19 Statement::Expression@21-22 IntLiteral@21-22',
    'say before a statement in an anonymous sub';
is placed(Q[sub { print 1, 2 }]),
    'StatementList@0-18 Statement::Expression@0-18 Sub@0-18 Blockoid@4-18 StatementList@6-16 Statement::Expression@6-16 Call::Name::WithoutParentheses@6-16 Name@6-11 ArgList@12-16 IntLiteral@12-13 IntLiteral@15-16',
    'print with two arguments in an anonymous sub';
