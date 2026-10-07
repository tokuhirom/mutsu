use Test;

# `Type::Simple` vs `Term::Name` for a bareword depends on what the innermost
# declaration of the name is, not on a unit-wide table. Every row was checked
# against `raku` first; this file must pass under both `raku` and `mutsu`.

plan 6;

sub kinds(Str $code) {
    $code.AST.gist.subst(/\s+/, ' ', :g).comb(/'Term::Name' | 'Type::Simple'/).join(',')
}

# A lexical enum variable shadows a class of the same name declared outside.
is kinds(Q|class Header { }; { my enum State <Start Header Done>; Header.WHAT }|),
    'Term::Name',
    'an enum variant in a block shadows an outer class';
is kinds(Q|my enum State <Start Header Done>; Header.WHAT|),
    'Term::Name',
    'an enum variant is a term';

# A constant holding a type object is a type; any other constant is a term.
is kinds(Q|my constant Foo = 5; Foo|), 'Term::Name', 'a constant holding a number is a term';
is kinds(Q|constant Foo = Int; Foo|), 'Type::Simple,Type::Simple',
    'a constant holding a type name is a type';
is kinds(Q|my constant E = Metamodel::EnumHOW.new_type(:name<E>, :base_type(Int)); E.^name|),
    'Type::Simple,Type::Simple,Type::Simple',
    'a constant holding a MOP-created type is a type';

is Q|my constant Foo = 5; Foo|.AST.EVAL, 5, 'a term constant evaluates';
