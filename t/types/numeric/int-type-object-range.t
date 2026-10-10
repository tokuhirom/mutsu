use Test;

# Found via the Audio::Icecast suite (t/020-stats-basic.t): `int.Range.max`
# must be the 64-bit maximum, not Inf.

plan 12;

is int.Range.min, -9223372036854775808, 'int.Range.min';
is int.Range.max, 9223372036854775807, 'int.Range.max';
is int64.Range.max, 9223372036854775807, 'int64.Range.max';
is int64.Range.min, -9223372036854775808, 'int64.Range.min';
is int.Range.max.WHAT.^name, 'Int', 'int.Range.max is an Int';
is int.Range.gist, '-9223372036854775808..9223372036854775807', 'int.Range.gist';
is long.Range.max, 9223372036854775807, 'long.Range.max';
is uint64.Range.max, 18446744073709551615, 'uint64.Range.max';
is int32.Range.gist, '-2147483648..2147483647', 'int32.Range';
is uint8.Range.gist, '0..255', 'uint8.Range';

my Int $x = int.Range.max;
is $x, 9223372036854775807, 'assignable to an Int variable';
is Int.Range.gist, '-Inf^..^Inf', 'Int.Range is still unbounded';
