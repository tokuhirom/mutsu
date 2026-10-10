use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 25;

my $callback = 'sub callback(&cb (Int --> Str)) { }'.AST;
my $param = $callback.statements[0].expression.signature.parameters[0];
isa-ok $param.sub-signature, RakuAST::Signature, 'callback has a sub-signature';
is $param.sub-signature.parameters.elems, 1, 'callback parameter count survives';
isa-ok $param.sub-signature.returns, RakuAST::Type::Simple, 'callback return type survives';
is $param.sub-signature.returns.name.parts[0].raku,
    'RakuAST::Name::Part::Simple.new("Str")', 'callback return type is Str';

is EVAL(q[
    multi choose(&cb:(Int)) { cb(4) }
    multi choose(&cb:(Str)) { cb('x') }
    choose(-> Int $n { $n * 3 })
].AST), 12, 'callback constraints select the Int candidate after lowering';
is EVAL(q[
    multi choose(&cb:(Int)) { cb(4) }
    multi choose(&cb:(Str)) { cb('x') }
    choose(-> Str $s { $s ~ '!' })
].AST), 'x!', 'callback constraints select the Str candidate after lowering';
throws-like { EVAL(q[sub apply(&cb:(Int)) { cb(1) }; apply(-> Str $s { $s })].AST) },
    X::TypeCheck::Binding::Parameter, 'callback mismatch is rejected after lowering';
throws-like { EVAL(q[-> &:(Int) {}({;})].AST) },
    X::TypeCheck::Binding::Parameter, 'single pointy callback retains its signature constraint';

my $unpack = 'sub unpack(+[Int $first, *@rest]) { $first + @rest.elems }'.AST;
my $unpack-param = $unpack.statements[0].expression.signature.parameters[0];
ok $unpack-param.slurpy === RakuAST::Parameter::Slurpy::SingleArgument,
    'anonymous slurpy unpack keeps its single-argument marker';
is $unpack-param.sub-signature.parameters.elems, 2, 'slurpy unpack keeps both nested parameters';
is EVAL(q[sub unpack(+[Int $first, *@rest]) { $first + @rest.elems }; unpack(7, 8, 9)].AST),
    9, 'anonymous slurpy collects arguments before unpacking';
is EVAL(q[sub unpack(*@ ($x, $y)) { $x + $y }; unpack(3, 4)].AST),
    7, 'anonymous array slurpy keeps its unpacking signature';

is EVAL(q[sub counted(+items where *.elems > 1) { items.elems }; counted(1, 2, 3)].AST),
    3, 'sigilless slurpy where accepts the collected list';
throws-like { EVAL(q[sub counted(+items where *.elems > 1) { items.elems }; counted(1)].AST) },
    X::TypeCheck::Binding::Parameter, 'sigilless slurpy where rejects the collected list';

is EVAL(q[sub named(:out([$x?, :$q = 7])) { "$x,$q" }; named(:out([3]))].AST),
    '3,7', 'named array unpacking retains nested defaults';
is EVAL(q[sub named(:out([$x, $y])) { $x + $y }; named(:out([3, 4]))].AST),
    7, 'named array unpacking binds both elements';
is EVAL(q[my $b = -> :($a, $b) { 42 }; $b.signature.gist].AST),
    '(:$ ($a, $b))', 'anonymous named unpacking keeps one signature level';
is EVAL(q[sub nested((:value((:key($x), :value($y))), |)) { $x + $y }; nested(1 => (3 => 4))].AST),
    7, 'nested named Pair unpacking retains both keys';
is EVAL(q[my $b = -> $x? { $x.raku }; $b()].AST), 'Mu',
    'optional block parameter retains its block default';
is EVAL(q[my @a; my $b = -> @x { @x.push(3) }; $b(@a); @a[0]].AST), 3,
    'single container parameter keeps caller identity';
throws-like { EVAL(q[my $b = -> $x --> Int { 'wrong' }; $b(1)].AST) },
    X::TypeCheck::Return, 'pointy block retains its return constraint';
ok !EVAL(q[sub separated($a;; $b) { }; &separated.signature.params[1].multi-invocant].AST),
    'dispatch boundary retains non-multi-invocant parameters';
is EVAL(q[my @seen; react whenever Supply.from-list((1, 2)) -> \v { @seen.push(v) }; @seen.join(',')].AST),
    '1,2', 'whenever keeps the sigilless pointy binding';

my $hand-built = RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => RakuAST::Sub.new(
        name => RakuAST::Name.from-identifier('inspect-callback'),
        signature => RakuAST::Signature.new(parameters => (
            RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new(name => '&cb'),
                optional => False,
                sub-signature => RakuAST::Signature.new(
                    parameters => (RakuAST::Parameter.new(
                        type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
                        optional => False),),
                    returns => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Str')))),)),
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new()))));
my $constructed;
lives-ok { $constructed = EVAL($hand-built) }, 'hand-built callback signature lowers';
ok $constructed.signature.params[0].sub_signature.returns === Str,
    'hand-built callback return type reaches runtime signature introspection';
