use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# A role's parameterization, header traits and `also does` in RakuAST,
# measured on rakudo 2026.09.

plan 27;

sub decl($src) { $src.AST.statements.tail.expression }

my $r = decl(Q|role R[::T, $x, Int :$y = 2] { }|);
isa-ok $r, RakuAST::Role, 'a parameterized role is a Role';
isa-ok $r.parameterization, RakuAST::Signature, 'with a Signature as its parameterization';
my @p = $r.parameterization.parameters;
is @p.elems, 3, 'holding every parameter';
is @p[0].type-captures[0].^name, 'RakuAST::Type::Capture', 'a ::T parameter is a type capture';
is @p[1].target.name, '$x', 'a positional parameter keeps its name';
nok @p[1].type.defined, 'without an implicit Type::Setting';
is @p[2].names, ('y',), 'a named parameter keeps its name';

my @t = decl(Q|role S { }; role R2 does S { }|).traits;
is @t.elems, 1, 'a header `does` is a trait';
isa-ok @t[0], RakuAST::Trait::Does, 'a Trait::Does';

my $pd = decl(Q|role S[::T] { }; role R3 does S[Int] { }|).traits[0];
isa-ok $pd.type, RakuAST::Type::Parameterized, 'a parameterized `does` is a Type::Parameterized';

my $is = decl(Q|class P { }; role R4 is P { }|).traits[0];
isa-ok $is, RakuAST::Trait::Is, 'a header `is` is a Trait::Is';
is decl(Q|role R5 is export { }|).traits[0].name.gist, 'RakuAST::Name.from-identifier("export")',
    '`is export` is a trait';

my $also = decl(Q|role S { }; role R6 { also does S }|).body.body.statement-list.statements
    .first(RakuAST::Statement::Also);
isa-ok $also, RakuAST::Statement::Also, '`also does` is a Statement::Also';
isa-ok $also.traits[0], RakuAST::Trait::Does, 'holding the Trait::Does';

is EVAL(Q|role R7[::T] { method t { T.^name } }; class C7 does R7[Int] { }; C7.new.t|.AST),
    'Int', 'a type capture survives the round trip';
is EVAL(Q|role R8[$x, :$y = 7] { method v { "$x $y" } }; class C8 does R8[3] { }; C8.new.v|.AST),
    '3 7', 'value parameters and their defaults survive it';
is EVAL(Q|role B9 { method b { 'b' } }; role R9 { also does B9 }; class C9 does R9 { }; C9.new.b|.AST),
    'b', '`also does` survives it';

my $also-parameterized = decl(Q|role R9a[$x] { }; class C9a { also does R9a[1 + 2] }|)
    .body.body.statement-list.statements.first(RakuAST::Statement::Also);
isa-ok $also-parameterized.traits[0].type, RakuAST::Type::Parameterized,
    '`also does` retains a parameterized role type';
isa-ok $also-parameterized.traits[0].type.args.args[0], RakuAST::ApplyInfix,
    'computed role arguments remain ordinary expressions';
is EVAL(Q|role R9b[$x] { method value { $x } }; class C9b { also does R9b[1 + 2] }; C9b.new.value|.AST),
    3, 'computed `also does` arguments survive EVAL and role composition';

my $adverb = decl(Q|role R10:ver<1.0>:auth<zef:test>[Bool :$enabled = False] {
    my sub label { 'role-body' }
    method state { ($enabled ?? 'yes' !! 'no') ~ '-' ~ label() }
}|);
isa-ok $adverb, RakuAST::Role, 'role adverbs and a parameterization remain a Role';
is $adverb.name.colonpairs.map(*.key).join(','), 'ver,auth',
    'role adverbs are colonpairs on its name';
is $adverb.parameterization.parameters.elems, 1,
    'the parameterization follows the adverb-bearing name';
is $adverb.parameterization.parameters[0].names, ('enabled',),
    'the parameter name survives after the role adverbs';
is $adverb.traits.elems, 0, 'name adverbs are not misread as role traits';
is EVAL(Q|role R11:ver<1.0>:auth<zef:test>[Bool :$enabled = False] {
    method state { $enabled ?? 'yes' !! 'no' }
}; class C11 does R11 { }; C11.new.state|.AST),
    'no', 'adverbs before a parameterization round-trip';
is EVAL(Q|role R12:ver<1.0> {
    my sub label { 'role-body' }
    method state { label() }
}; (class :: does R12 { }).new.state|.AST),
    'role-body', 'an adverb-bearing role keeps body lexicals in scope';
