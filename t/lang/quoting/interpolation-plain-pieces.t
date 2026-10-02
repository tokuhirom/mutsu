use Test;

# A plain `Str` or `Int` interpolated piece takes a fast path in the
# interpolation op (#10951); values that only look like one -- an allomorph,
# a role mixin -- must still render through their own `Str`.
plan 8;

my $i = 42;
my $s = "x";
is "key-$i", "key-42", 'Int piece';
is "$s-$i-$s", "x-42-x", 'Str and Int pieces';
is "{-9223372036854775807 - 1}", "-9223372036854775808", 'the most negative Int';
is "{2**70}", "1180591620717411303424", 'a big Int still renders';
is "{0}", "0", 'zero';
is "{<042>}", "042", 'an IntStr allomorph renders its string, not its Int';

my $m = 5 but role { method Str { "five" } };
is "$m", "five", 'a role-mixed Int renders through its own Str';

my @keys = (^3).map({ "k$_" });
is @keys.join(","), "k0,k1,k2", 'interpolation inside a map';
