use Test;

plan 3;

# Indexing a Range by an array of positions computes `start + i` per index.
# When the Range's bound sits near `i64::MAX` (reachable from a 128-bit
# address computation such as Net::Netmask's IPv6 `nth`, which indexes
# `($!start..$!end)` with a huge `$!start`), that add can overflow a plain
# `i64` -- an out-of-range index either way, so it must answer `Nil` rather
# than crashing the interpreter.

my $start = 9223372036854775804;  # i64::MAX - 3
my $end   = 9223372036854775807;  # i64::MAX
my @n = 0, 10;
my @got = ($start..$end)[@n];
is @got[0], $start, 'an in-range index near i64::MAX still resolves';
ok !@got[1].defined, 'an index whose start+i overflows i64 answers Nil, not a crash';

lives-ok { ($start..$end)[@n] }, 'indexing a Range past i64::MAX never panics';
