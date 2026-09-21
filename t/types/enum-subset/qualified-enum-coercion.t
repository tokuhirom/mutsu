use Test;

module QualifiedEnumCoercion {
    enum Level <Low High>;
}

plan 2;
my $value = QualifiedEnumCoercion::Level('High');
isa-ok $value, QualifiedEnumCoercion::Level, 'a qualified enum call resolves its declared enum';
is $value.key, 'High', 'the qualified enum call selects the requested key';
