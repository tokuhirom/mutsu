use Test;

plan 6;

# `$buf[i]++`/`--` had no Buf/Blob-aware branch in the increment/decrement
# read-modify-write path (unlike plain element assignment, which already
# writes through the native storage node) -- so it silently read Nil and lost
# the write, leaving the buffer unchanged. Found via Acme::Anguish, whose
# esolang interpreter builds a `Buf[uint8]` "tape" via `$stack[$ptr]++`.
my $b = Buf[uint8].new: 0;
$b[0]++;
$b[0]++;
is $b.raku, 'Buf[uint8].new(2)', '$buf[i]++ mutates the element in place';

$b[0]--;
is $b.raku, 'Buf[uint8].new(1)', '$buf[i]-- mutates the element in place';

# Autovivifies past the current length, like plain element assignment does.
my $b2 = Buf[uint8].new: 1, 2;
$b2[5]++;
is $b2.raku, 'Buf[uint8].new(1,2,0,0,0,1)',
    '$buf[i]++ past the end autovivifies with zero fill';

# Untyped Buf behaves the same way.
my $b3 = Buf.new: 0;
$b3[0]++;
is $b3.raku, 'Buf.new(1)', 'untyped $buf[i]++ mutates in place';

# A read-only Blob refuses the mutation.
my $blob = Blob.new: 1, 2, 3;
dies-ok { $blob[0]++ }, 'Blob[i]++ dies: Blob is immutable';

# Truncates to the element width, exactly as `[i] = v` already does.
my $b4 = Buf[uint8].new: 254;
$b4[0]++;
$b4[0]++;
is $b4.raku, 'Buf[uint8].new(0)',
    '$buf[i]++ truncates to the element width on overflow';
