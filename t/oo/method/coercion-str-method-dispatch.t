use Test;

plan 3;

class CoercionStrClass is Cool {
    has $.a = 'coerced';
    method Str() { $.a }
}

class DerivedFromStrForCoercion is Str {
    has $.a;
}

is Str(CoercionStrClass.new), 'coerced', 'Str(...) uses the object .Str method';
isa-ok DerivedFromStrForCoercion.new.Str, DerivedFromStrForCoercion, '.Str preserves Str subclasses';
isa-ok DerivedFromStrForCoercion.new.Str, Str, '.Str result remains a Str';
