use Test;

# A role's parameterization, header traits and `also does` in RakuAST,
# measured on rakudo 2026.09.

plan 17;

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
