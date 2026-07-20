use v6;
use Test;

# String bitwise compound assignment (`~&=`, `~|=`, `~^=`) is the only path that
# reaches the VM's StrBitAnd/Or/Xor opcodes (plain `~&` uses the infix builtin).
# The opcode used to operate byte-wise and pad every operator to the *longer*
# length, so `$x ~&= "ab"` on "abc" produced "ab\0" instead of "ab". It now
# delegates to the same `str_bitwise_op` the builtin uses: codepoint-wise,
# truncating to the shorter operand for `~&` and padding to the longer for
# `~|` / `~^`, with NFC normalization.

plan 12;

# ~&= truncates to the shorter operand (no null padding).
{
    my $x = "abc";
    $x ~&= "ab";
    is $x, ("abc" ~& "ab"), '~&= matches the infix builtin';
    is $x.chars, 2, '~&= truncates to the shorter operand';
}
{
    my $x = "ab";
    $x ~&= "abcd";
    is $x.chars, 2, '~&= truncates regardless of which side is shorter';
}

# ~|= pads to the longer operand.
{
    my $x = "abcd";
    $x ~|= "ab";
    is $x, ("abcd" ~| "ab"), '~|= matches the infix builtin';
    is $x.chars, 4, '~|= pads to the longer operand';
}

# ~^= pads to the longer operand.
{
    my $x = "abcd";
    $x ~^= "ab";
    is $x, ("abcd" ~^ "ab"), '~^= matches the infix builtin';
    is $x.chars, 4, '~^= pads to the longer operand';
}

# Codepoint-wise, not byte-wise: a multi-byte char must not be split.
{
    my $x = "café";
    $x ~&= "ab";
    is $x, ("café" ~& "ab"), '~&= is codepoint-wise for multi-byte input';
    is $x.chars, 2, '~&= counts codepoints, not bytes';
}

# Concrete value checks (independent of the builtin, against known results).
{
    my $x = "abc";
    $x ~|= "AB";
    is $x, "abc", '~|= concrete value (lowercase bits already set)';
}
{
    my $x = "abc";
    $x ~&= "abc";
    is $x, "abc", '~&= equal-length identity';
}
{
    my $x = "abc";
    $x ~^= "abc";
    is $x.ords.all, 0, '~^= of equal strings is all-NUL';
}
