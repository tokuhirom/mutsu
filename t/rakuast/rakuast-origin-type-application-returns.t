use Test;
use experimental :rakuast;

# Origins of a type application (`Array[Int]`, `Hash[Str, Array[Int]]`) as a
# term, a declaration's and a parameter's type, and of a signature's return
# part (`--> Int`, `--> 42`, `--> Str:D`) on routines and pointy blocks, as
# rakudo 2026.11 places them (#15780): the base `Type::Simple` of an
# application spans all of it, the `ArgList` the text between the brackets,
# and a `Signature` runs to the end of its return part.

plan 11;

sub walk(Str $src) {
    my @l;
    sub w($n) {
        with $n.origin { @l.push($n.^name.subst('RakuAST::', '') ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

is walk(Q[my $x = Array[Int];]),
    'StatementList@0-19 Statement::Expression@0-18 VarDeclaration::Simple@0-18 Initializer::Assign@6-18 Type::Parameterized@8-18 Type::Simple@8-18 Name@8-13 ArgList@14-17 Type::Simple@14-17 Name@14-17',
    'origins of my $x = Array[Int];';
is walk(Q[my Array[Int] $x;]),
    'StatementList@0-17 Statement::Expression@0-16 VarDeclaration::Simple@0-16 Type::Parameterized@3-13 Type::Simple@3-13 Name@3-8 ArgList@9-12 Type::Simple@9-12 Name@9-12',
    'origins of my Array[Int] $x;';
is walk(Q[sub f(Hash[Str, Int] $h) { }]),
    'StatementList@0-28 Statement::Expression@0-28 Sub@0-28 Name@4-5 Signature@6-23 Parameter@6-23 Type::Parameterized@6-20 Type::Simple@6-20 Name@6-10 ArgList@11-19 Type::Simple@11-14 Name@11-14 Type::Simple@16-19 Name@16-19 ParameterTarget::Var@21-23 VarDeclaration::Simple@21-23 Type::Parameterized@6-20 Type::Simple@6-20 Name@6-10 ArgList@11-19 Type::Simple@11-14 Name@11-14 Type::Simple@16-19 Name@16-19 Blockoid@25-28 StatementList@26-26',
    'origins of sub f(Hash[Str, Int] $h) { }';
is walk(Q[my $y = Hash[Str,Array[Int]];]),
    'StatementList@0-29 Statement::Expression@0-28 VarDeclaration::Simple@0-28 Initializer::Assign@6-28 Type::Parameterized@8-28 Type::Simple@8-28 Name@8-12 ArgList@13-27 Type::Simple@13-16 Name@13-16 Type::Parameterized@17-27 Type::Simple@17-27 Name@17-22 ArgList@23-26 Type::Simple@23-26 Name@23-26',
    'origins of my $y = Hash[Str,Array[Int]];';
is walk(Q[sub f(--> Int) { }]),
    'StatementList@0-18 Statement::Expression@0-18 Sub@0-18 Name@4-5 Signature@6-13 Type::Simple@10-13 Name@10-13 Blockoid@15-18 StatementList@16-16',
    'origins of sub f(--> Int) { }';
is walk(Q[sub g($x --> Str ) { }]),
    'StatementList@0-22 Statement::Expression@0-22 Sub@0-22 Name@4-5 Signature@6-16 Parameter@6-8 ParameterTarget::Var@6-8 VarDeclaration::Simple@6-8 Type::Simple@13-16 Name@13-16 Blockoid@19-22 StatementList@20-20',
    'origins of sub g($x --> Str ) { }';
is walk(Q[sub h( --> 42) { }]),
    'StatementList@0-18 Statement::Expression@0-18 Sub@0-18 Name@4-5 Signature@7-13 IntLiteral@11-13 Blockoid@15-18 StatementList@16-16',
    'origins of sub h( --> 42) { }';
is walk(Q[my method m(Int $a --> Str:D) { }]),
    'StatementList@0-33 Statement::Expression@0-33 Method@0-33 Name@10-11 Signature@12-28 Parameter@12-18 Type::Simple@12-15 Name@12-15 ParameterTarget::Var@16-18 VarDeclaration::Simple@16-18 Type::Simple@12-15 Name@12-15 Type::Definedness@23-28 Type::Simple@23-28 Name@23-26 Blockoid@30-33 StatementList@31-31',
    'origins of my method m(Int $a --> Str:D) { }';
is walk(Q[sub k(--> Array[Int]) { }]),
    'StatementList@0-25 Statement::Expression@0-25 Sub@0-25 Name@4-5 Signature@6-20 Type::Parameterized@10-20 Type::Simple@10-20 Name@10-15 ArgList@16-19 Type::Simple@16-19 Name@16-19 Blockoid@22-25 StatementList@23-23',
    'origins of sub k(--> Array[Int]) { }';
is walk(Q[my $c = -> $x --> Int { 1 };]),
    'StatementList@0-28 Statement::Expression@0-27 VarDeclaration::Simple@0-27 Initializer::Assign@6-27 PointyBlock@8-27 Signature@11-21 Parameter@11-13 ParameterTarget::Var@11-13 VarDeclaration::Simple@11-13 Type::Simple@18-21 Name@18-21 Blockoid@22-27 StatementList@24-25 Statement::Expression@24-25 IntLiteral@24-25',
    'origins of my $c = -> $x --> Int { 1 };';
is walk(Q[my $d = -> --> Int { 1 };]),
    'StatementList@0-25 Statement::Expression@0-24 VarDeclaration::Simple@0-24 Initializer::Assign@6-24 PointyBlock@8-24 Signature@11-18 Type::Simple@15-18 Name@15-18 Blockoid@19-24 StatementList@21-22 Statement::Expression@21-22 IntLiteral@21-22',
    'origins of my $d = -> --> Int { 1 };';
