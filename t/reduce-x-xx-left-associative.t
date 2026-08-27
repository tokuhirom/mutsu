use v6;
use Test;

plan 15;

# `x` (list/string repeat) and `xx` (list repeat, list result) are
# LEFT-associative in Raku, exactly like `~`. A [x]/[xx] reduce over 3+
# operands must fold left-to-right: `[x] "a", 2, 3` is `("a" x 2) x 3`
# = "aa" x 3 = "aaaaaa" (6 characters) -- NOT `"a" x (2 x 3)` = "a" x 222.

is (([x] "a", 2, 3)), "aaaaaa", '[x] reduce folds left-associatively (3 operands)';
is (([x] "a", 2, 3)).chars, 6, '[x] reduce produces 6 chars, not 222';
is (([x] "a", 2)), "aa", '[x] reduce over 2 operands matches infix x';
is (([x] "z")), "z", '[x] reduce over 1 operand is the identity element';

# `xx` list-repeats; folding left-to-right nests the earlier repeats.
is (([xx] 1, 2, 3)).gist, "((1 1) (1 1) (1 1))", '[xx] reduce folds left-associatively';

# Triangle (partial-results) reduce must show the same left-fold order:
# ("a", "a" x 2, ("a" x 2) x 3).
is-deeply (([\x] "a", 2, 3)), ("a", "aa", "aaaaaa"), '[\\x] triangle reduce folds left-associatively';

# `x=` (compound assignment) already left-folds via repeated single ops;
# pin it stays correct alongside the reduce fix.
my $s = "a";
$s x= 3;
is $s, "aaa", 'x= compound assignment still works';

# reduce() with the actual &infix:<x> callable must agree with [x].
is reduce(&infix:<x>, "a", 2, 3), "aaaaaa", 'reduce(&infix:<x>, ...) folds left-associatively';

# Neighbouring reduce operators must NOT have flipped associativity.
is (([~] "a", "b", "c")), "abc", '[~] reduce unaffected (associative op)';
is (([**] 2, 3, 2)), 512, '[**] reduce stays right-associative (2**(3**2))';
isnt (([**] 2, 3, 2)), 64, '[**] reduce is not the left-assoc reading';

my $pair = [=>] 1, 2, 3;
is $pair.key, 1, '[=>] reduce stays right-associative (key)';
is $pair.value.key, 2, '[=>] reduce stays right-associative (nested pair key)';
is $pair.value.value, 3, '[=>] reduce stays right-associative (nested pair value)';

is (([-] 10, 3, 2)), 5, '[-] reduce stays left-associative ((10-3)-2)';
