use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

my $absolute = Q[role Child is ::Exception { }].AST.statements[0].expression.traits[0].type;
isa-ok $absolute, RakuAST::Type::Simple, 'an absolute parent is a simple type';
isa-ok $absolute.name.parts[0], RakuAST::Name::Part::Empty, 'the leading empty name part is retained';
isa-ok $absolute.name.parts[1], RakuAST::Name::Part::Simple, 'the parent identifier remains a separate name part';

my $parameterized = Q[role R[$m] { }; class C does R['append'] { }].AST.statements[1].expression.traits[0].type;
isa-ok $parameterized, RakuAST::Type::Parameterized, 'a role application is a parameterized type';
isa-ok $parameterized.args.args[0], RakuAST::QuotedString, 'a string argument stays an expression';
is $parameterized.args.args[0].segments[0].value, 'append', 'the string argument retains its value';

my $does-export = Q[role Parent { }; role Child does Parent is export { }].AST.statements[1].expression;
is $does-export.traits.map(*.^name).join('|'),
    'RakuAST::Trait::Does|RakuAST::Trait::Is', 'does before export keeps its source order';
my $export-does = Q[role Parent { }; role Child is export does Parent { }].AST.statements[1].expression;
is $export-does.traits.map(*.^name).join('|'),
    'RakuAST::Trait::Is|RakuAST::Trait::Does', 'export before does keeps its source order';

is EVAL(Q[
    module HeaderScope {
        role Parent { method marker { 'parent' } }
        role Child does Parent is export { }
        class Composed does Child { }
    }
    HeaderScope::Composed.new.marker
].AST), 'parent', 'a role with mixed header traits composes after AST lowering';

is EVAL(Q[
    role MessageRole[$message] { method message { $message } }
    class MessageClass does MessageRole['hello'] { }
    MessageClass.new.message
].AST), 'hello', 'a string-valued role application survives AST evaluation';

is EVAL(Q[
    role NumberRole[$number] { method number { $number } }
    class NumberClass does NumberRole[17] { }
    NumberClass.new.number
].AST), 17, 'a numeric role argument remains a value';

is EVAL(Q[
    role TypedRole[::T] { method type { T.^name } }
    class TypedClass does TypedRole[IO::Path] { }
    TypedClass.new.type
].AST), 'IO::Path', 'qualified type-valued role arguments keep their binding';

ok EVAL(Q[
    class AbsoluteBase { }
    module AbsoluteScope {
        role AbsoluteBase is ::AbsoluteBase { method marker { 'role' } }
        class Child does AbsoluteBase { }
    }
    AbsoluteScope::Child.new ~~ AbsoluteBase
].AST), 'an absolute parent resolves outside the declaring role namespace';

ok EVAL(Q[
    my role ScalarRole { }
    sub compose($value) { $value does ScalarRole }
    my $value = 10;
    compose($value);
    so $value ~~ ScalarRole
].AST), 'does on a readonly scalar parameter updates the caller container';

ok EVAL(Q[
    my role ElementRole { }
    sub compose($value) { $value does ElementRole }
    my @values = 4, 10;
    compose(@values[1]);
    so @values[1] ~~ ElementRole && !(@values[0] ~~ ElementRole)
].AST), 'does writes back to exactly the supplied array element';

ok EVAL(Q[
    my role HashRole { }
    sub compose($value) { $value does HashRole }
    my %values = x => 10;
    compose(%values<x>);
    so %values<x> ~~ HashRole
].AST), 'does retains the hash element source';

is EVAL(Q[
    role InitializerRole { has $.value }
    (1 but InitializerRole(7)).value
].AST), 7, 'the direct call spelling is a role initializer';

throws-like {
    EVAL(Q[role GroupedRole { has $.value }; 1 but (GroupedRole(7))].AST)
}, X::Coerce::Impossible, 'a grouped role call remains an ordinary coercion';

ok EVAL(Q[
    my role ClosureRole { }
    my &compose = sub ($value) { $value does ClosureRole };
    my $value = 10;
    compose($value);
    so $value ~~ ClosureRole
].AST), 'an anonymous sub retains does caller-container metadata';

ok EVAL(Q[
    my role PointyRole { }
    my &compose = -> $value { $value does PointyRole };
    my $value = 10;
    compose($value);
    so $value ~~ PointyRole
].AST), 'a pointy closure keeps the parameter metadata needed by does';

ok EVAL(Q[
    my role MethodRole { }
    class Composer { method compose($value) { $value does MethodRole } }
    my $value = 10;
    Composer.new.compose($value);
    so $value ~~ MethodRole
].AST), 'a method does writeback through the same compiled binding path';

my role ConstructedRole { }
my $sub = Q[sub constructed($value) { $value does ConstructedRole }].AST.statements[0].expression;
my $constructed = RakuAST::Sub.new(
    name => $sub.name, signature => $sub.signature, body => $sub.body
);
my &constructed = EVAL($constructed);
my $constructed-value = 10;
constructed($constructed-value);
ok $constructed-value ~~ ConstructedRole,
    'a hand-built routine recomputes does binding metadata from its body';

role ConstructedParent { method marker { 'constructed' } }
my $constructed-role = RakuAST::Role.new(
    name => RakuAST::Name.from-identifier('ConstructedChild'),
    traits => (
        RakuAST::Trait::Does.new(RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('ConstructedParent')
        )),
        RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier('rw')),
    ),
    body => RakuAST::RoleBody.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new()))
);
is EVAL($constructed-role).new.marker, 'constructed',
    'a hand-built role accepts mixed composition and rw traits';

nok Q[role { }].AST.gist.contains('role-declaration-name'),
    'anonymous declaration provenance stays out of the public AST rendering';

is EVAL(Q[
    class PseudoType { method kind(::?CLASS $other) { $other.^name } }
    PseudoType.new.kind(PseudoType.new)
].AST), 'PseudoType', 'absolute type support retains the distinct pseudo-type spelling';

isa-ok RakuAST::Role.new, RakuAST::Package, 'constructed roles retain their package ancestry';
isa-ok RakuAST::RoleBody.new, RakuAST::Sub, 'a constructed role body retains its routine ancestry';
isa-ok RakuAST::Role.new.body, RakuAST::RoleBody, 'an omitted role body defaults to an empty role body';
isa-ok RakuAST::RoleBody.new.body, RakuAST::Blockoid, 'an omitted role blockoid defaults to an empty blockoid';
isa-ok RakuAST::Role.new.parameterization, RakuAST::Signature, 'an omitted role signature has its model default';

done-testing;
