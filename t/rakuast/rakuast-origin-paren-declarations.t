use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a `.=` initializer
# (`my Int $c .= new`) runs from the `.=` and places its call; a lone
# declaration or assignment in parentheses (`(my Int $a = 1)`, `($s = 5)`)
# runs over the text between them; `(...).=new(42)` places its operator and
# call after the parenthesized left side.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class eq 'StatementList' }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my Int $c .= new;]),
    'Statement::Expression@0-16 VarDeclaration::Simple@0-16 Type::Simple@3-6 Name@3-6 Initializer::CallAssign@10-16 Call::Method@13-16 Name@13-16 ArgList@16-16',
    'a .= initializer';
is placed(Q[my $x = (my Int $a = 1);]),
    'Statement::Expression@0-23 VarDeclaration::Simple@0-23 Initializer::Assign@6-23 Circumfix::Parentheses@8-23 SemiList@9-22 Statement::Expression@9-22 VarDeclaration::Simple@9-22 Type::Simple@12-15 Name@12-15 Initializer::Assign@19-22 IntLiteral@21-22',
    'a declaration in parentheses';
is placed(Q[(my Int $b .= new).=new(42);]),
    'Statement::Expression@0-27 ApplyDottyInfix@0-27 Circumfix::Parentheses@0-18 SemiList@1-17 Statement::Expression@1-17 VarDeclaration::Simple@1-17 Type::Simple@4-7 Name@4-7 Initializer::CallAssign@11-17 Call::Method@14-17 Name@14-17 ArgList@17-17 DottyInfix::CallAssign@18-20 Call::Method@20-27 Name@20-23 ArgList@24-26 IntLiteral@24-26',
    '.= on a parenthesized .= declaration';
is placed(Q[my $s; say(($s = 5));]),
    'Statement::Expression@0-5 VarDeclaration::Simple@0-5 Statement::Expression@7-20 Call::Name@7-20 Name@7-10 ArgList@11-19 Circumfix::Parentheses@11-19 SemiList@12-18 Statement::Expression@12-18 ApplyInfix@12-18 Assignment@15-16 ArgList@12-18 Var::Lexical@12-14 IntLiteral@17-18',
    'an assignment in parentheses';
