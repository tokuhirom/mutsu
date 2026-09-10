use Test;

# An enum value's `.WHICH` identity is `{EnumType}|{ordinal}` (the position in
# the enum), a ValueObjAt. Previously enum values fell to a global-counter
# WHICH fallback, producing a non-deterministic `Int|N` that broke value
# identity (`Bob.WHICH eqv Bob.WHICH` was False).

plan 10;

enum Names <Bob Alice Carol>;
is Bob.WHICH.Str, 'Names|0', 'enum WHICH is EnumType|ordinal';
is Alice.WHICH.Str, 'Names|1', 'second enum value WHICH';
is Carol.WHICH.Str, 'Names|2', 'third enum value WHICH';
is Bob.WHICH.^name, 'ValueObjAt', 'enum WHICH is a value type (ValueObjAt)';

# Identity is deterministic and stable.
ok Bob.WHICH eqv Bob.WHICH, 'same enum value has equal WHICH';
nok Bob.WHICH eqv Alice.WHICH, 'different enum values have distinct WHICH';

# The ordinal, not the underlying value, drives WHICH.
enum Color (red => 5, green => 9);
is red.WHICH.Str, 'Color|0', 'valued enum WHICH uses the ordinal, not the value';
is green.WHICH.Str, 'Color|1', 'second valued enum WHICH ordinal';

# String-valued enum.
enum Foo (a => "x", b => "y");
is a.WHICH.Str, 'Foo|0', 'string-valued enum WHICH ordinal';
is b.WHICH.Str, 'Foo|1', 'string-valued enum WHICH second ordinal';

done-testing;
