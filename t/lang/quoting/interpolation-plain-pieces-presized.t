use Test;

# An interpolation whose pieces are all plain Strs and Ints is joined where
# the pieces sit, with its buffer sized up front. It must render exactly what
# the general path renders: decimal Ints at both extremes, NFC across a seam,
# big Str pieces, and empty pieces.

plan 10;

my $i = 42;
is "key-$i", 'key-42', 'Str and Int';
my $neg = -9223372036854775807 - 1;
is "min $neg max {9223372036854775807}", 'min -9223372036854775808 max 9223372036854775807',
    'i64 extremes';
my $z = 0;
is "$z$z$z", '000', 'several Int pieces';

my $e = 'e';
my $acute = "\x[301]";
is "$e$acute".chars, 1, 'a combining mark composes across the seam';
is "$e$acute", "\x[e9]", 'the result is NFC';

my $big = 'x' x 300;
my $s = "[$big|$i]";
is $s.chars, 305, 'a big Str piece';
is $s.substr(0, 2) ~ $s.substr(*-4), '[x|42]', 'the big piece keeps its neighbours';

my $empty = '';
is "$empty$i$empty", '42', 'empty Str pieces';

my @keys = (1..3).map({ "k$_" });
is-deeply @keys, ['k1', 'k2', 'k3'], 'in a map';

my %h;
%h{"key-$_"} = $_ for 1..3;
is %h<key-2>, 2, 'as a hash key';
