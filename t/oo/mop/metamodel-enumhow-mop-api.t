use Test;

# The `Metamodel::EnumHOW` MOP API (#9866): an enum built through
# `new_type` / `add_enum_value` / `compose_values` rather than the `enum`
# declarator, as in `raku-doc`'s `Type/Metamodel/EnumHOW.rakudoc`.

plan 27;

{
    my constant E = Metamodel::EnumHOW.new_type(:name<E>, :base_type(Int));
    is E.^name, 'E', 'new_type names the type';
    is E.HOW.^name, 'Perl6::Metamodel::EnumHOW', 'the type reports EnumHOW';
    is E.^is_composed, 0, 'not composed before .^compose';
    is E.^elems, 0, 'no values yet';
    is-deeply E.^enum_values, {}, 'enum_values is empty';

    E.^add_role(NumericEnumeration);
    E.^compose;
    is E.^is_composed, 1, 'composed after .^compose';
    E.^add_enum_value("Warning" => 0);
    E.^add_enum_value("Failure" => 1);
    E.^compose_values;

    is-deeply E.^enum_values, {Failure => 1, Warning => 0},
        'enum_values maps each key to its value';
    is E.^elems, 2, 'elems counts the added values';
    is-deeply E.^enum_from_value(1), (Failure => 1),
        'enum_from_value answers the value object that was added';
    is E.^enum_from_value(1).gist, 'Failure => 1', 'its gist is the Pair';
    ok E.^enum_from_value(5) =:= Mu, 'an unknown value answers Mu';
    is E.^enum_value_list.map(*.key).join(','), 'Warning,Failure',
        'enum_value_list keeps the insertion order';
}

{
    my @called;
    my constant F = Metamodel::EnumHOW.new_type(:name<F>, :base_type(Str));
    F.^add_role(StringyEnumeration);
    F.^compose;
    F.^set_export_callback(-> { @called.push('cb') });
    F.^add_enum_value("a" => "x");
    F.^compose_values;
    F.^compose_values;
    is @called.elems, 1, 'compose_values runs the export callback exactly once';
    is-deeply F.^enum_values, {a => "x"}, 'a stringy MOP enum';
    is F.^enum_from_value("x").key, 'a', 'enum_from_value on a Str value';
}

# The doc's "roughly equivalent" BEGIN block.
BEGIN {
    my constant Error = Metamodel::EnumHOW.new_type: :name<Error>, :base_type(Int);
    Error.^add_role: Enumeration;
    Error.^add_role: NumericEnumeration;
    Error.^compose;
    for <Warning Failure Exception Sorrow Panic>.kv -> Int $v, Str $k {
        Error.^add_enum_value: $k => $v;
        OUR::{$k} := Error.^enum_from_value: $v;
    }
    Error.^compose_values;
    OUR::<Error> := Error;
}
is-deeply Error.^enum_values,
    {Exception => 2, Failure => 1, Panic => 4, Sorrow => 3, Warning => 0},
    'the doc BEGIN-block enum has all five values';
is-deeply OUR::<Sorrow>, (Sorrow => 3), 'OUR:: holds the bound value object';

# `add_enum_value` et al. exist on EnumHOW only.
{
    class C { }
    dies-ok { C.^add_enum_value("a" => 1) }, 'add_enum_value is EnumHOW-only';
}

# The marker roles every declared numeric / stringy enum does.
{
    enum Col <R G>;
    enum Sty (sa => "x");
    enum Arr (aa => [1]);
    is NumericEnumeration.^name, 'NumericEnumeration', 'NumericEnumeration is a type';
    is StringyEnumeration.^name, 'StringyEnumeration', 'StringyEnumeration is a type';
    ok R ~~ NumericEnumeration, 'an Int enum value does NumericEnumeration';
    ok Col ~~ NumericEnumeration, 'an Int enum type does NumericEnumeration';
    nok R ~~ StringyEnumeration, 'an Int enum value is not stringy';
    ok sa ~~ StringyEnumeration, 'a Str enum value does StringyEnumeration';
    nok Sty ~~ NumericEnumeration, 'a Str enum type is not numeric';
    nok aa ~~ NumericEnumeration | StringyEnumeration, 'an Array enum does neither';
    sub takes-numeric(NumericEnumeration $x) { $x.key }
    is takes-numeric(G), 'G', 'a NumericEnumeration parameter accepts an Int enum value';
}
