use Test;

# ADR-11276 §9.145: `Any`'s `Any(Enumeration)`, `Any(Bool)` and `Any:U`
# candidates of `pairs`, `keys`, `values`, `kv`, `antipairs` and `invert` are
# rows. An enum's type object and each of its values list the whole
# enumeration (`self.enums.METHOD`); a defined value's `kv` is its own key and
# value; any other type object lists nothing. The order is the declaration
# order (Rakudo's is its `Map`'s), so the tests sort where Rakudo's would vary.

plan 30;

enum Color <red green blue>;

# The type object.
is Color.keys.sort.join(','), 'blue,green,red', 'type object keys';
is Color.values.sort.join(','), '0,1,2', 'type object values';
is Color.pairs.sort(*.value).map({ .key ~ '=' ~ .value }).join(','), 'red=0,green=1,blue=2',
    'type object pairs';
is Color.kv.elems, 6, 'type object kv lists every key and value';
is Color.antipairs.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), '0=red,1=green,2=blue',
    'type object antipairs';
is Color.invert.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), '0=red,1=green,2=blue',
    'type object invert';
isa-ok Color.pairs, Seq, 'a view is a Seq';
isa-ok Color.values[0], Int, 'values are the raw values';
isa-ok Color.keys[0], Str, 'keys are strings';

# A value lists its whole enumeration, except `kv`, which is its own.
is red.keys.sort.join(','), 'blue,green,red', 'value keys';
is red.pairs.elems, 3, 'value pairs';
is red.antipairs.elems, 3, 'value antipairs';
is red.invert.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), '0=red,1=green,2=blue',
    'value invert';
is red.kv.join(','), 'red,0', 'a value\'s kv is its own key and value';

# A positional value: `invert` lists its elements, the views list the
# enumeration rather than the value's elements.
my Array enum Sizes (:small[1, 2], :big[3]);
is Sizes.invert.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), '1=small,2=small,3=big',
    'invert lists a positional value\'s elements';
is small.keys.sort.join(','), 'big,small', 'a positional value\'s keys are the enumeration\'s';
is small.pairs.elems, 2, 'a positional value\'s pairs are the enumeration\'s';
is small.invert.elems, 3, 'a positional value inverts the enumeration';
is small.elems, 2, 'elems is still the positional value\'s';

# `Bool` is the core's enumeration.
is Bool.keys.sort.join(','), 'False,True', 'Bool.keys';
is Bool.pairs.sort(*.value).map({ .key ~ '=' ~ .value }).join(','), 'False=0,True=1',
    'Bool.pairs';
is Bool.invert.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), '0=False,1=True',
    'Bool.invert';
is True.invert.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), '0=False,1=True',
    'True.invert';
is True.kv.elems, 4, 'True.kv lists the enumeration';
is False.values.sort.join(','), '0,1', 'False.values';

# Any other type object lists nothing.
is Int.pairs.elems, 0, 'Int.pairs';
is Hash.keys.elems, 0, 'Hash.keys';
is Int.invert.elems, 0, 'Int.invert';

# Not methods of an enum: `.enum_value_list` is the metaobject's.
throws-like { Color.enum_value_list }, X::Method::NotFound, 'no enum_value_list method';
throws-like { "Color".invert.eager }, X::TypeCheck, 'a Str naming an enum is a Str';
