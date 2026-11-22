use experimental :rakuast;
use Test;

# `also` statements of a class body (#18598), as Rakudo 2026.11 keeps them:
# `also is rw;` is a `Statement::Also` holding `Trait::Is(rw)`, not a trait
# of the class, and every `also` statement is placed with its trait.

plan 9;

sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        else { @l.push($class) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
sub first-statement($src) { $src.AST.statements[0].expression.body.body.statement-list.statements[0] }

my $class = Q[class A { also is rw; has $.x }].AST.statements[0].expression;
is $class.traits.elems, 0, 'also is rw is no trait of the class';
my $also = first-statement(Q[class B { also is rw; has $.x }]);
is $also.^name, 'RakuAST::Statement::Also', 'it is a Statement::Also';
is $also.traits[0].name.canonicalize, 'rw', 'holding is rw';
is $also.origin.from ~ '-' ~ $also.origin.to, '10-20', 'placed over its text';

is placed(Q[class E { also is rw; has $.x }]),
    'Class@0-31 Name@6-7 Block@8-31 Blockoid@8-31 Trait::Is@15-20 Name@18-20 VarDeclaration::Simple@22-29',
    'also is rw';
is placed(Q[class F { also is Int; }]),
    'Class@0-24 Name@6-7 Block@8-24 Blockoid@8-24 Trait::Is@15-21 Type::Simple@18-21 Name@18-21',
    'also is a parent';
is placed(Q[role R {}; class D { also does R; }]),
    'Role@0-9 Name@5-6 RoleBody@5-9 Name@5-6 Signature Parameter Blockoid@7-9 Role::ResolveInstantiations Statement::Expression Nqp Role::MetaObject Nqp Class@11-35 Name@17-18 Block@19-35 Blockoid@19-35 Trait::Does@26-32 Type::Simple@31-32 Name@31-32',
    'also does a role';

is EVAL(Q[class C2 { also is rw; has $.x }; my $o = C2.new; $o.x = 5; $o.x].AST), 5,
    'EVAL of the tree keeps the class rw';
class C3 { also is rw; has $.x }
my $o = C3.new;
$o.x = 7;
is $o.x, 7, 'also is rw still makes the class rw';
