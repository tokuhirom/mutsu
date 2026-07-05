use v6;
use Test;

plan 7;

# A global match (`m:g`, `.match(:g)`, `~~ m:g`) must preserve the nested/folded
# positional captures in each Match object, exactly like a single match does.
# Previously the multi-match path used a string-only Match builder, so a folded
# group such as `(\d) ** 4 % '.'` collapsed `$m[0]` to a bare string.

my @ip;
@ip.push(.list) for "127.0.0.1 5.6.7.8" ~~ m:g/ (\d ** 1..3) ** 4 % '.' /;

is +@ip, 2, 'two top-level m:g matches';

my $first = @ip[0][0];
is $first.Str, "127 0 0 1", 'folded group stringifies to the joined sub-matches';
is $first.elems, 4, 'folded group has 4 sub-captures';
is $first[0].Str, "127", 'first sub-capture';
is $first[3].Str, "1", 'last sub-capture';

my $second = @ip[1][0];
is $second[1].Str, "6", 'second match folded sub-capture';

# A simple quantified capture via m:g also folds.
my @words;
@words.push(.[0].elems) for "aXbXc dXe" ~~ m:g/ (\w) ** 2..3 % 'X' /;
is-deeply @words, [3, 2], 'm:g folds a plain quantified capture group';
