use Test;

plan 14;

# #12990: .^can and .^methods see the methods a built-in class gets from a
# composed role.
is Sub.^can('of').elems, 2, 'Sub.^can("of"): Block and Callable';
is Block.^can('of').elems, 2, 'Block.^can("of")';
is Routine.^can('of').elems, 2, 'Routine.^can("of")';
is Code.^can('of').elems, 1, 'Code.^can("of"): only the Callable one';

sub of-count(Mu \t, Str $name) { t.^methods.grep(*.name eq $name).elems }

is of-count(Hash, 'of'), 2, 'Hash.^methods lists Hash.of and Associative.of';
is of-count(Hash, 'keyof'), 2, 'Hash.^methods lists both keyof';
is of-count(List, 'of'), 1, 'List.^methods lists Positional.of';
is of-count(Map, 'of'), 1, 'Map.^methods lists Associative.of';
is of-count(Map, 'keyof'), 1, 'Map.^methods lists Associative.keyof';
is of-count(Pair, 'keyof'), 1, 'Pair.^methods lists Associative.keyof';
is of-count(Block, 'of'), 2, 'Block.^methods lists Block.of and Callable.of';
is-deeply Map.^methods.grep(*.name eq 'of').map(*.package.^name).List, ('Associative',),
    'the role is the package of the flattened method';
is List.^can('of').elems, 1, 'List.^can("of") unchanged';
is Hash.^can('of').elems, 2, 'Hash.^can("of") unchanged';
