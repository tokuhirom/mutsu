use Test;

plan 12;

multi sub d(int $x) { 'native' }
multi sub d(Int $x) { 'boxed' }
multi sub s(str $x) { 'native-str' }
multi sub s(Str $x) { 'boxed-str' }
multi sub n(num $x) { 'native-num' }
multi sub n(Num $x) { 'boxed-num' }

my $rt = 0;

# rakudo constant-folds before dispatch runs, so a folded expression ranks on
# the native candidate exactly as the literal does.
is d(5), 'native', 'an integer literal ranks native';
is d(5 + 0), 'native', 'a folded integer expression ranks native';
is d(2 * 3), 'native', 'a folded product ranks native';
is d(-(2 + 1)), 'native', 'a folded negation ranks native';
is s('a' ~ 'b'), 'native-str', 'a folded string concatenation ranks native';
is n(1e0 + 1e0), 'native-num', 'a folded Num sum ranks native';

# A runtime operand is not foldable and stays boxed.
is d(5 + $rt), 'boxed', 'a runtime operand ranks boxed';
is d($rt), 'boxed', 'a plain variable ranks boxed';

# A fold that leaves the native width is a BigInt, not an int.
is d(2**35 * 2**35), 'boxed', 'a fold past the native width ranks boxed';

# The folded value itself is unchanged.
is (5 + 0), 5, 'the folded value is still correct';
is ('a' ~ 'b'), 'ab', 'the folded string is still correct';
is (2**35 * 2**35), 2**70, 'the folded big product is still correct';
