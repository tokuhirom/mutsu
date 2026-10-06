use lib 't/lib';
use Test;

# Names and literal values in RakuAST, measured on rakudo 2026.09. A bareword
# is resolved at parse time, so the node says what the name is: a type is a
# `Type::Simple`, a constant or enum value a `Term::Name`, an `our sub`
# reached by its qualified name a `Call::Name`. The names of a unit's own
# nested declarations and of a `use`d module count. A version and a whole
# complex number are literals of their own, and `need` / `import` are
# statements.

plan 34;

sub exprs($src) { $src.AST.statements.map(*.expression) }

# --- nested declarations are reached by their composed names
{
    my @e = exprs(Q[class Outer { class Inner { }; method m { Outer::Inner } }; Outer::Inner.new]);
    like @e[0].gist, /'Type::Simple.new(' \s* 'RakuAST::Name.from-identifier-parts("Outer","Inner")'/,
        'a class nested in a class is Outer::Inner';
    my @m = exprs(Q[module M1 { class C { }; our sub foo { 1 }; our constant c = 5 }; M1::C.new; M1::foo; M1::c]);
    isa-ok @m[1].operand, RakuAST::Type::Simple, 'M1::C is a type';
    isa-ok @m[2], RakuAST::Call::Name, 'an our sub by its qualified name is a Call::Name';
    unlike @m[2].gist, /args/, 'with no args';
    isa-ok @m[3], RakuAST::Term::Name, 'an our constant by its qualified name is a Term::Name';
    is @m[3].name.canonicalize, 'M1::c', 'with the qualified name';
    my @p = exprs(Q[package P1 { enum Status <S1 S2> }; P1::Status::S1]);
    isa-ok @p[1], RakuAST::Term::Name, 'an enum value of a nested enum is a Term::Name';
    is @p[1].name.canonicalize, 'P1::Status::S1', 'by its full name';
}

# --- definite types and pseudo-packages
{
    my @d = exprs(Q[Str:D; Int:U]);
    isa-ok @d[0], RakuAST::Type::Definedness, '`Str:D` is a Type::Definedness';
    ok @d[0].definite, 'definite';
    nok @d[1].definite, '`Int:U` is not';
    isa-ok exprs(Q[CORE::DateTime])[0], RakuAST::Type::Simple, '`CORE::DateTime` is a type';
    isa-ok exprs(Q[GLOBAL])[0], RakuAST::Type::Simple, '`GLOBAL` is a type';
}

# --- what a `use` brought in
{
    my @s = Q[use lib 't/lib'; use RakuastNames; RakuastNamesCls.new; RAKUAST-K; rn-a; RakuastEnum::rn-b].AST.statements;
    isa-ok @s[2].expression.operand, RakuAST::Type::Simple, 'an imported class is a type';
    isa-ok @s[3].expression, RakuAST::Term::Name, 'an imported constant is a Term::Name';
    isa-ok @s[4].expression, RakuAST::Term::Name, 'an imported enum value is a Term::Name';
    is @s[5].expression.name.canonicalize, 'RakuastEnum::rn-b', 'and so is its qualified spelling';
}

# --- need and import
{
    my @s = Q[use lib 't/lib'; need RakuastNames; import RakuastNames; import RakuastNames :ALL].AST.statements;
    isa-ok @s[1], RakuAST::Statement::Need, '`need` is a Statement::Need';
    is @s[1].module-names[0].canonicalize, 'RakuastNames', 'with the module name';
    isa-ok @s[2], RakuAST::Statement::Import, '`import` is a Statement::Import';
    is @s[2].module-name.canonicalize, 'RakuastNames', 'with the module name';
    nok @s[2].argument.defined, 'no argument without a tag';
    isa-ok @s[3].argument, RakuAST::ColonPair::True, 'a tag is a ColonPair::True';
}

# --- version and complex literals
{
    my @e = exprs(Q[v6.d; v1.2.3+; <1+2i>]);
    isa-ok @e[0], RakuAST::VersionLiteral, '`v6.d` is a VersionLiteral';
    is @e[0].value, v6.d, 'with its version';
    is @e[1].value, v1.2.3+, 'a `+` version keeps its plus';
    isa-ok @e[2], RakuAST::ComplexLiteral, '`<1+2i>` is a ComplexLiteral';
    is @e[2].value, <1+2i>, 'with its value';
    like @e[2].gist, /'ComplexLiteral.new(<1+2i>)'/, 'rendered as its angle-bracket literal';
}

# --- the round trip computes what the parsed program does
sub run($src) { EVAL($src.AST) }
is run(Q[module M2 { our sub foo { 41 } }; M2::foo]), 41, 'a qualified our sub';
is run(Q[module M3 { our constant c = 5 }; M3::c]), 5, 'a qualified our constant';
is run(Q[Str:D.^name]), 'Str:D', 'a definite type object';
is run(Q[use lib 't/lib'; need RakuastNames; import RakuastNames; RAKUAST-K]), 3, '`need` then `import`';
ok run(Q[v6.d]) === v6.d, 'a version literal';
