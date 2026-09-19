use Test;

plan 3;

class ParameterizedHolder {
    has Hash[Array, Str] $.value;
}

lives-ok { ParameterizedHolder.new(:value(Hash[Array, Str])) },
    'a parameterized type object satisfies the same typed attribute';
ok Hash[Array, Str] ~~ Hash[Array, Str],
    'a parameterized type object smartmatches its exact type';
nok Hash[Array, Str] ~~ Hash[Array, Any],
    'multi-parameter type objects keep their arguments invariant';
