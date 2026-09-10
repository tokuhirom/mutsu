use v6;
use Test;

# `constant` inlining + constant-condition branch resolution (ADR-0006 §2.2).
# A constant whose value is a compile-time scalar is read straight from the
# constant pool, and an if/unless on one resolves its branch at compile time.
# Every case here must be indistinguishable from the un-inlined compilation.

plan 16;

constant DEBUG = False;
constant LIMIT = 60 * 60;
constant NAME  = 'mutsu';

# --- reads ----------------------------------------------------------------
is LIMIT, 3600, 'a constant folds its own initializer';
is LIMIT + 1, 3601, 'an inlined constant folds into the surrounding expression';
is NAME ~ '!', 'mutsu!', 'a Str constant inlines';
is DEBUG, False, 'a Bool constant inlines';
is LIMIT.WHAT.^name, 'Int', 'the inlined value keeps its type';

# A constant read inside a sub body (a child compiler) still inlines.
sub step($x) {
    if DEBUG {
        die 'the dead branch must never run';
    }
    $x * 2 + LIMIT
}
is step(1), 3602, 'a sub body inlines an outer constant';

# --- constant conditions --------------------------------------------------
my $ran = 0;
if DEBUG { $ran = 1 }
is $ran, 0, 'if on a False constant does not run its body';

if DEBUG { $ran = 1 } else { $ran = 2 }
is $ran, 2, 'the else branch of a False constant runs';

unless DEBUG { $ran = 3 }
is $ran, 3, 'unless on a False constant runs its body';

constant ON = True;
if ON { $ran = 4 } else { $ran = 5 }
is $ran, 4, 'if on a True constant runs the then branch';

# An elsif chain still resolves.
if DEBUG      { $ran = 6 }
elsif LIMIT   { $ran = 7 }
else          { $ran = 8 }
is $ran, 7, 'an elsif chain resolves through a constant condition';

# --- a dead branch that declares something is still compiled ---------------
# raku installs declarations at compile time, so a never-taken branch must not
# simply be discarded when it contains one.
if DEBUG {
    my $never = 1;
    $ran = $never;
}
is $ran, 7, 'a dead branch with a declaration does not run, and does not disturb the rest';

# --- shadowing ------------------------------------------------------------
{
    constant DEBUG = True;
    my $inner = 0;
    if DEBUG { $inner = 1 }
    is $inner, 1, 'an inner constant shadows the outer one inside its block';
}
my $outer = 0;
if DEBUG { $outer = 1 }
is $outer, 0, 'the outer constant is back in effect after the block';

constant SHADOW = 1;
{
    my $SHADOW = 2;
    is $SHADOW, 2, 'an ordinary `my` of the same bare name is not replaced by the constant';
}
is SHADOW, 1, 'the constant is unaffected by the shadowing `my`';

done-testing;
