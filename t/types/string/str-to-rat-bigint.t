use Test;

plan 10;

# Str.Rat / Str.FatRat must parse with big-integer precision: the old parser
# went through i64 and collapsed anything past it to 0 (DBIish reads DECIMAL
# columns back as strings and converts with .Rat / .FatRat).

is "18446744073709551616".Rat, 18446744073709551616, 'Str.Rat past uint64';
is "-18446744073709551616".Rat, -18446744073709551616, 'negative Str.Rat past uint64';
is "1208925819614629174706176.1".Rat.Str, '1208925819614629174706176.1',
    'Str.Rat with big integer part keeps the fraction';
is "9223372036854775807".Rat, 9223372036854775807, 'i64 max still exact';
is "0.123456789012345678901".FatRat.Str, '0.123456789012345678901',
    'Str.FatRat keeps 21 fractional digits';
is "18446744073709551616.5".FatRat.Str, '18446744073709551616.5',
    'Str.FatRat with big integer part';
is "3.14".Rat, 3.14, 'small decimal unchanged';
is "2/4".Rat.raku, '0.5', 'fraction form unchanged';
is "1e3".Rat, 1000, 'scientific form unchanged';
is "-2.5e-2".Rat, -0.025, 'negative scientific form unchanged';
