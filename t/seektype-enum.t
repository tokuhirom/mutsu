use Test;

# The built-in SeekType enum (used by IO::Handle.seek and user IO classes that
# subclass it, e.g. IO::Blob). Regression: its values were bare Str, so a
# `SeekType:D $whence = SeekFromBeginning` default failed its type check.

plan 12;

is SeekFromBeginning.Int, 0, 'SeekFromBeginning is 0';
is SeekFromCurrent.Int,   1, 'SeekFromCurrent is 1';
is SeekFromEnd.Int,       2, 'SeekFromEnd is 2';

ok SeekFromBeginning ~~ SeekType, 'SeekFromBeginning smartmatches SeekType';
ok SeekFromCurrent   ~~ SeekType, 'SeekFromCurrent smartmatches SeekType';
ok SeekFromEnd       ~~ SeekType, 'SeekFromEnd smartmatches SeekType';

# Qualified names resolve to the same values.
is SeekType::SeekFromEnd.Int, 2, 'qualified SeekType::SeekFromEnd works';

# A SeekType:D parameter accepts the enum value, including as a default.
sub whence(SeekType:D $w = SeekFromBeginning) { $w.Int }
is whence(),            0, 'SeekType:D default value passes its type check';
is whence(SeekFromEnd), 2, 'SeekType:D parameter accepts an explicit value';

# Native IO::Handle.seek accepts the enum values.
my $p = "tmp/seektype-enum-test.txt".IO;
$p.spurt("0123456789");
my $fh = $p.open;
$fh.seek(3, SeekFromBeginning);
is $fh.read(2).decode, '34', 'seek SeekFromBeginning + read';
$fh.seek(2, SeekFromCurrent);
is $fh.read(1).decode, '7', 'seek SeekFromCurrent + read';
$fh.seek(-1, SeekFromEnd);
is $fh.read(1).decode, '9', 'seek SeekFromEnd + read';
$fh.close;
$p.unlink;
