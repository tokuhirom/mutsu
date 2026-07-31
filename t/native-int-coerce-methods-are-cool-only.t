# `42.int8` / `"42".byte` are coercion methods Rakudo declares on `Cool`. mutsu
# answered them for every native integer *type* name and on every value, so
# `.^can('bool')` was true for anything at all. Code that probes
# `$obj.^can($field)` before moving on to the next candidate — which is exactly
# what Template::Mustache's context lookup does — then called a method that
# silently answered 0 instead of missing, and the answer depended on hash order,
# so a section rendered empty about half the time.
#
# Every assertion here also passes unmodified under rakudo.
use Test;
plan 14;

# The Raku-native integer types are coercion methods, on Cool values.
is 300.int8, 44, 'Int.int8 coerces';
is "300".byte, 44, 'Str.byte coerces';
ok (42.^can('int8')), 'Int can int8';
ok ("42".^can('byte')), 'Str can byte';
ok ((1, 2).^can('int8')), 'List is Cool, so it can int8';

# The NativeCall::Types C-width aliases name a type but are not methods.
for <bool long ulong longlong ulonglong size_t ssize_t atomicint> -> $name {
    nok (42.^can($name)), "Int cannot $name -- it is a type, not a method";
}

# ...and a value that is not Cool has none of them. `Pair` is the one that bit
# us: the Mustache context stack is a stack of Pairs.
nok ((a => 1).^can('int8')), 'Pair is not Cool, so it cannot int8';
