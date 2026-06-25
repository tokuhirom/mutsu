use Test;

# An ENTER phaser that is the textually-last statement of a block provides the
# block's result value (Raku semantics). Previously the value was lost because
# the ENTER section runs before the body-result baseline is recorded, so a
# trailing ENTER yielded Nil.

plan 18;

# Sub whose only statement is an ENTER phaser.
sub only-enter() { ENTER 'SANDMAN' }
is only-enter(), 'SANDMAN', 'sole ENTER phaser is the sub return value';

# ENTER as last statement after other statements.
sub enter-last() { my $x = 1; ENTER 'X' }
is enter-last(), 'X', 'trailing ENTER after a statement is the return value';

# A non-phaser statement after ENTER wins.
sub value-last() { ENTER 'X'; 'Y' }
is value-last(), 'Y', 'a trailing non-phaser statement wins over ENTER';

# Earlier non-phaser, then a trailing ENTER: ENTER wins.
sub enter-after-value() { 'Y'; ENTER 'X' }
is enter-after-value(), 'X', 'trailing ENTER wins over an earlier value';

# Multiple ENTERs: the textually-last one provides the value.
sub two-enters() { ENTER 'X'; ENTER 'Z' }
is two-enters(), 'Z', 'last of several ENTER phasers provides the value';

# ENTER as an r-value in mainline (block form and bare value).
is (ENTER { 42 }), 42, 'ENTER block as an r-value (mainline)';
is (ENTER 7), 7, 'ENTER value as an r-value (mainline)';

# ENTER side effects still happen at entry time.
my $log = '';
sub side-effect() { ENTER { $log ~= 'entered '; 'val' } }
my $r = side-effect();
is $r, 'val', 'ENTER block value returned';
is $log, 'entered ', 'ENTER body ran at entry';

# Bare block (statement) with a trailing ENTER as its value.
my $b = do { my $q = 3; ENTER 'BV' };
is $b, 'BV', 'do-block with trailing ENTER yields the ENTER value';

# KEEP/UNDO success determination: a phaser-only block (no value statement) is
# unsuccessful, so UNDO fires, not KEEP.
my $str;
{
    KEEP { $str ~= 'K1 ' }
    KEEP { $str ~= 'K2 ' }
    UNDO { $str ~= 'U1 ' }
    UNDO { $str ~= 'U2 ' }
}
is $str, 'U2 U1 ', 'phaser-only block (no value) runs UNDO';

# A block ending with a truthy value runs KEEP.
my $str2;
{
    KEEP { $str2 ~= 'K1 ' }
    UNDO { $str2 ~= 'U1 ' }
    1;
}
is $str2, 'K1 ', 'block ending in a truthy value runs KEEP';

# A block ending in a falsy value runs UNDO.
my $str3;
{
    KEEP { $str3 ~= 'K1 ' }
    UNDO { $str3 ~= 'U1 ' }
    0;
}
is $str3, 'U1 ', 'block ending in a falsy value runs UNDO';

# ENTER value does not leak out of a nested block (stack discipline).
sub nested-enter() {
    my $inner = do { ENTER 'inner' };
    "outer:$inner"
}
is nested-enter(), 'outer:inner', 'nested ENTER value does not corrupt outer result';

# Closures (block / pointy) with a trailing ENTER provide the return value.
my $cb = { ENTER 5 };
is $cb(), 5, 'block closure with sole ENTER returns the ENTER value';

my $pb = -> { my $x = 1; ENTER 'L' };
is $pb(), 'L', 'pointy closure with trailing ENTER returns the ENTER value';

# A closure ending in a value (after a phaser) returns that value via the stack.
my $kb = { KEEP {}; 7 };
is $kb(), 7, 'closure with KEEP phaser still returns its trailing value';

# do-block ENTER value used inline in a larger expression.
is "v=" ~ (do { ENTER 'BV' }), 'v=BV', 'do-block ENTER value usable inline';
