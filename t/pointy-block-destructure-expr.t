use v6;
use Test;

# Pointy blocks used as an *expression* (assigned to a variable or called
# inline) must support destructuring sub-signatures, exactly like the `for`
# loop pointy form and like `sub f([$a,$b])` / `sub f(% [:$k])`.

plan 12;

# Hash destructure assigned to a &-sigil variable (the original zef-surfaced bug).
my &hb = -> % [:$k, :@v] { "$k @v[]" };
is hb(%(k => 1, v => [2, 3])), "1 2 3", 'hash destructure -> &var';

# Hash destructure assigned to a $-sigil variable.
my $sb = -> % [:$k] { $k };
is $sb(%(k => 42)), 42, 'hash destructure -> $var';

# Array (positional) destructure assigned to a &-sigil variable.
my &ab = -> [$a, $b] { "$a $b" };
is ab([1, 2]), "1 2", 'array destructure -> &var';

# Standalone inline-called pointy with array destructure.
is (-> [$a, $b] { "$a $b" })([9, 8]), "9 8", 'inline array destructure';

# Standalone inline-called pointy with hash destructure.
is (-> % [:$k] { $k })(%(k => 7)), 7, 'inline hash destructure';

# Named-array destructure inside the hash sub-signature.
my &nb = -> % [:$name, :@deps] { "$name: @deps[]" };
is nb(%(name => 'Foo', deps => <a b c>)), "Foo: a b c", 'hash destructure with named array';

# @-sigil param followed by a bracket sub-signature.
my &mb = -> @a [$x, $y] { "$x/$y" };
is mb([5, 6]), "5/6", 'array param with bracket sub-signature';

# Mixed: destructure param followed by a plain param.
my &gb = -> [$a, $b], $c { "$a $b $c" };
is gb([1, 2], 3), "1 2 3", 'destructure param plus plain param';

# .map with an array-destructuring pointy block.
is-deeply [[1, 2], [3, 4]].map(-> [$a, $b] { $a + $b }).List, (3, 7),
    '.map array destructure';

# Paren sub-signature still works (regression guard).
my &pb = -> ($a, $b) { "$a $b" };
is pb((1, 2)), "1 2", 'paren sub-signature still works';

# Plain single/multi params still work (regression guard).
my &f1 = -> $x { $x * 2 };
is f1(21), 42, 'plain single param still works';
my &f2 = -> $a, $b { $a + $b };
is f2(3, 4), 7, 'plain multi param still works';
