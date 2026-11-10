use Test;
use lib 't/lib';
use LastWrapperSub;

plan 6;

# An imported routine named `last` / `next` shadows the loop-control
# statement, bare or called; `&CORE::last()` still performs the real `last`.
my @r = (1..10).map({ last if $_ == 4; $_ });
is-deeply @r, [1, 2, 3], "last still ends the map through the wrapper";
is $LastWrapperSub::counter, 1, "the imported last was called (bare)";

my @t = (1..10).map({ last() if $_ == 4; $_ });
is-deeply @t, [1, 2, 3], "last() works too";
is $LastWrapperSub::counter, 2, "the imported last was called (parens)";

my $out = '';
for 1..5 { next if $_ == 2; $out ~= $_ }
is $out, "1345", "next through the wrapper skips one iteration";
is $LastWrapperSub::counter, 12, "the imported next was called";
