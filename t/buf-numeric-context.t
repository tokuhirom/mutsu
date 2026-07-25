use v6;
use Test;

plan 6;

# A Buf/Blob is Positional, so it numifies to its element count — `+$buf` and
# `.Numeric` already agreed; the infix numeric comparisons did not, and read it
# as 0. (HTTP-UserAgent's TestServer gates its response on
# `$in-buf.subbuf($header-end + 4) == $length`.)

my $b = Buf.new(1, 2, 3, 4, 5);

is +$b, 5, 'prefix + gives the element count';
is $b.Numeric, 5, '.Numeric gives the element count';
ok $b == 5, 'infix == compares the element count';
nok $b == 4, 'and is False for a different count';

my $blob = Blob.new(9, 9);
ok $blob == 2, 'a Blob numifies the same way';

my $empty = Buf.new;
ok $empty == 0, 'an empty Buf numifies to 0';
