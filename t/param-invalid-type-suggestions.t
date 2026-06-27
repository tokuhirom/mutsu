use Test;

plan 8;

# A type-only parameter naming an undeclared type is X::Parameter::InvalidType,
# and enum values declared in the same unit are offered as suggestions.
throws-like q|enum E <RT123926Foo Bar>; sub x(RT123926Floo) {}|,
    X::Parameter::InvalidType,
    typename    => 'RT123926Floo',
    suggestions => ['RT123926Foo'],
    'enum value names are suggested for a mistyped type-only param';

# A bare unknown type-only parameter (no enum around) still throws InvalidType.
throws-like 'sub y(Junctoin) {}',
    X::Parameter::InvalidType,
    typename => 'Junctoin',
    'undeclared bare type-only param throws InvalidType';

# Uppercase value-terms are valid bare value-params, not rejected as types.
lives-ok { EVAL 'sub a(Inf)  {}' }, 'Inf is a valid value-param';
lives-ok { EVAL 'sub b(NaN)  {}' }, 'NaN is a valid value-param';

# A real enum value used as a value-param is accepted.
lives-ok { EVAL 'enum C <Red Green>; sub c(Red) {}' },
    'an enum value is a valid value-param';

# A declared class used as a type-only param is accepted.
lives-ok { EVAL 'class Foo {}; sub d(Foo) {}' },
    'a declared class is a valid type-only param';

# A genuine literal value-param is never treated as a type.
lives-ok { EVAL 'sub e(42) {}' },  'a numeric literal value-param is fine';
lives-ok { EVAL 'sub f("x") {}' }, 'a string literal value-param is fine';
