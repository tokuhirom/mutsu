use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a `proto` routine places
# its name, signature and parameters as a `sub` does, and its `{*}` body is
# an `OnlyStar` over the braces (or inside a written block).

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
is placed(Q[proto sub g($) {*}]),
    'Sub@0-18 Name@10-11 Signature@12-13 Parameter@12-13 ParameterTarget::Var@12-13 VarDeclaration::Anonymous@12-13 OnlyStar@15-18',
    'an anonymous parameter';
is placed(Q[proto sub h(Int $x --> Int) {*}]),
    'Sub@0-31 Name@10-11 Signature@12-26 Parameter@12-18 Type::Simple@12-15 Name@12-15 ParameterTarget::Var@16-18 VarDeclaration::Simple@16-18 Type::Simple@12-15 Name@12-15 Type::Simple@23-26 Name@23-26 OnlyStar@28-31',
    'a typed parameter and a return type';
is placed(Q[class C { proto method m() {*} }]),
    'Class@0-32 Name@6-7 Block@8-32 Blockoid@8-32 Method@10-30 Name@23-24 Signature@25-25 OnlyStar@27-30',
    'a proto method';
is placed(Q[proto sub z(|) { {*} }]),
    'Sub@0-22 Name@10-11 Signature@12-13 Parameter@12-13 Blockoid@15-22 OnlyStar@17-20',
    'a block holding {*}';
