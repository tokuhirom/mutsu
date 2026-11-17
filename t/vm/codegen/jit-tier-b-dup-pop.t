use v6;
use Test;

# Tier B inline `Dup` / `Pop` (#15737): every `&&` / `||` / `?? !!` / `//`
# chain compiles to `Dup`, a conditional jump and `Pop`. In a JIT-compiled
# body a refcount-free top word (small Int, Num, Bool, Package) is copied or
# dropped in native code; every other word (Str, Nil, a heap Int, an array,
# a lazy list that a sinking `Pop` must drain) takes the interpreter arm.
# The hot loops below run each shape past the JIT threshold; CI's jit-stress
# job re-runs this file with MUTSU_JIT_THRESHOLD=2.

plan 9;

# 1. A Bool chain: the shape of a character-class predicate.
sub is-name-char(Str $ch --> Bool:D) {
    return False if $ch eq '';
    my $ord = $ch.ord;
    (0x41 <= $ord <= 0x5A) || (0x61 <= $ord <= 0x7A) || (0x30 <= $ord <= 0x39)
        || $ch eq '-' || $ch eq '_' || $ch eq ':' || $ch eq '.';
}
my $n = 0;
for ^50 {
    for 'aZ9-_:.!? é'.comb -> $c { $n++ if is-name-char($c) }
}
is $n, 50 * 7, 'a Bool || / && chain answers the same in a hot body';

# 2. `||` / `&&` returning the deciding Int / Num operand.
sub first-true(Int $a, Int $b) { $a || $b }
sub both(Num $a, Num $b) { $a && $b }
my ($i, $f) = 0, 0e0;
for ^200 { $i += first-true(0, $_) + first-true($_, 7); $f += both(1e0, 0.5e0) }
is $i, (^200).sum * 2 + 7, 'Int operands of || are kept, not booleanized';
is $f, 100e0, 'Num operands of && are kept';

# 3. Boxed words (Str) through the same chain shape take the step path.
sub pick-str(Str $a, Str $b) { $a || $b }
my $s = '';
for ^200 { $s = pick-str('', 'x' ~ $_) }
is $s, 'x199', 'a Str operand of || stays intact';

# 4. A big Int (not a small inline Int word) through `||`.
sub big-or(Int $a) { $a || 1 }
my $big = 0;
for ^200 { $big = big-or(2 ** 70 + $_) }
is $big, 2 ** 70 + 199, 'a heap-allocated Int survives Dup / Pop';

# 5. `//` with Nil and a type object (Package word) on the left.
sub def-or($a) { $a // 'dflt' }
my @seen;
for ^200 { @seen[0] = def-or(Nil); @seen[1] = def-or(Int); @seen[2] = def-or(0) }
is-deeply @seen, ['dflt', 'dflt', 0], '// over Nil, a type object and 0';

# 6. Ternary on Bool and Int conditions.
sub tern($x) { $x ?? 'y' !! 'n' }
my $t = '';
for ^200 { $t = tern($_ %% 2) ~ tern($_) ~ tern(True) ~ tern(0e0) }
is $t, 'nyyn', 'ternaries over Bool / Int / Num conditions';

# 7. A statement-level lazy list is still sunk (drained) by Pop.
my $drained = 0;
sub sink-lazy() { (gather { $drained++; take 1 }).lazy; 1 }
for ^200 { sink-lazy() }
is $drained, 200, 'a sunk gather in a hot body is still drained';

# 8. An array (heap word) as a chain operand.
sub arr-or(@a) { @a || [0] }
my $last;
for ^200 { $last = arr-or([$_]) }
is-deeply $last, [199], 'an array operand of || is passed through';
