use Test;

# Introspection reads Rakudo's method-table snapshot one owner at a time
# (#14878); the answers must not depend on which owner is asked first, nor on
# ordinary method calls having run before.

plan 14;

# Ordinary calls on plain values come first and must not change what follows.
is "abc".uc, 'ABC', 'a Str method call';
is 5.abs, 5, 'an Int method call';
is Buf.new(1, 2).elems, 2, 'a Buf method call';

# A Cool-only method on a buffer is still refused, on a plain Str still answered.
throws-like { Buf.new(1, 2).uc }, X::Method::NotFound, 'a Buf has no uc';
is "abc".chars, 3, 'a Str has chars';

# `.^can` per owner, in an order unrelated to the snapshot's.
ok Str.^can('uc'), 'Str can uc';
ok Rat.^can('numerator'), 'Rat can numerator';
nok Rat.^can('no-such-method'), 'Rat cannot no-such-method';
ok Int.^can('abs'), 'Int can abs';

# `.^methods` lists names.
ok Str.^methods.map(*.name).grep('chars'), 'Str.^methods lists chars';
ok Int.^methods.map(*.name).grep('FatRat'), 'Int.^methods lists FatRat';

# The augment conflict reads the declared / only lists of one owner.
throws-like { EVAL 'use MONKEY-TYPING; augment class Str { method uc { "x" } }' }, Exception,
    'redeclaring Str.uc is refused';
lives-ok { EVAL 'use MONKEY-TYPING; augment class Str { method frobnicate-lazy-facts { 1 } }' },
    'a new name on Str is accepted';
is "x".frobnicate-lazy-facts, 1, 'and callable';
