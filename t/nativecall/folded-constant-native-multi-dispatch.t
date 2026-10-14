use Test;

plan 12;

multi sub d(int $x) { 'native' }
multi sub d(Int $x) { 'boxed' }
multi sub s(str $x) { 'native-str' }
multi sub s(Str $x) { 'boxed-str' }
multi sub n(num $x) { 'native-num' }
multi sub n(Num $x) { 'boxed-num' }

my $rt = 0;

# rakudo 2026.09 dispatches a literal, and so a folded expression, as a boxed
# value: it ranks the boxed candidate (#13065).
is d(5), 'boxed', 'an integer literal ranks boxed';
is d(5 + 0), 'boxed', 'a folded integer expression ranks boxed';
is d(2 * 3), 'boxed', 'a folded product ranks boxed';
is d(-(2 + 1)), 'boxed', 'a folded negation ranks boxed';
is s('a' ~ 'b'), 'boxed-str', 'a folded string concatenation ranks boxed';
is n(1e0 + 1e0), 'boxed-num', 'a folded Num sum ranks boxed';

# A runtime operand is not foldable and stays boxed.
is d(5 + $rt), 'boxed', 'a runtime operand ranks boxed';
is d($rt), 'boxed', 'a plain variable ranks boxed';

# A fold that leaves the native width is a BigInt, not an int.
is d(2**35 * 2**35), 'boxed', 'a fold past the native width ranks boxed';

# The folded value itself is unchanged.
is (5 + 0), 5, 'the folded value is still correct';
is ('a' ~ 'b'), 'ab', 'the folded string is still correct';
is (2**35 * 2**35), 2**70, 'the folded big product is still correct';
