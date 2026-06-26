use Test;

# Raku subscript protocol: a positional subscript coerces the index via its
# `.Int` method. `@a[$obj]` where `$obj` defines `method Int` was returning Nil
# (mutsu did not coerce the Instance index at all). The fix also drains the
# coercion method's captured-outer writeback into the caller's slot (§D(b)
# Slice F, retain-on-miss), mirroring the coercion/render redispatch fixes.

plan 7;

# --- basic Int-coerced index ---
{
    my @a = 10, 20, 30;
    class CI { method Int { 1 } }
    is @a[CI.new], 20, '@a[$obj] coerces index via .Int';
}

# --- Int method captured-outer writeback propagates ---
{
    my $calls = 0;
    my @a = 10, 20, 30;
    class CW { method Int { $calls++; 2 } }
    my $w = CW.new;
    my $x = @a[$w];
    my $y = @a[$w];
    is $x, 30, 'indexed value correct';
    is $calls, 2, '.Int captured-outer write propagates (subscript)';
}

# --- Int coercion on a list literal / word list ---
{
    my @a = <a b c d>;
    class CN { method Int { 3 } }
    is @a[CN.new], 'd', 'Int coercion picks the right element';
}

# --- associative subscript is NOT affected (string key stays a key) ---
{
    my %h = alpha => 'A', beta => 'B';
    is %h<alpha>, 'A', 'hash key lookup unaffected by index coercion';
}

# --- plain integer index still works (no regression) ---
{
    my @a = 100, 200, 300;
    is @a[2], 300, 'plain integer subscript unchanged';
}

# --- Int-coerced subscript inside a sibling BUILD keeps parent BUILD write
#     (regression guard: relies on the BUILDALL retain-on-miss fix, #3620) ---
{
    my $pc = 0;
    class IdxI { method Int { 0 } }
    class IdxP { submethod BUILD { $pc++ } }
    class IdxC is IdxP { submethod BUILD { my @a = 7, 8; my $x = @a[IdxI.new] } }
    IdxC.new;
    is $pc, 1, 'index coercion inside child BUILD keeps parent BUILD write';
}
