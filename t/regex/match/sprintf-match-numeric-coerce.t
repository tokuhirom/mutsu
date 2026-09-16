use Test;
plan 8;

# Regression for https://github.com/tokuhirom/mutsu/issues/8584: sprintf/.fmt()
# numeric directives used to only dispatch a *user-defined* .Int/.Numeric
# method (via `has_user_method`), which skips Match's own native .Int/.Numeric
# coercion entirely — every numeric directive silently formatted a Match as 0.

"255" ~~ /(\d+)/;
my $m = $0;

is $m.fmt("%08b"), "11111111", '.fmt on a Match dispatches native .Int (%b)';
is sprintf("%08b", $m), "11111111", 'sprintf on a Match dispatches native .Int (%b)';
is sprintf("%d", $m), "255", 'sprintf %d on a Match dispatches native .Int';
is "%d".sprintf($m), "255", '.sprintf method form on a Match dispatches native .Int';

# Hyper method call: each element goes through the same 1-arg `.fmt` dispatch
# the scalar case above does.
my @ms = ("1" ~~ /(\d+)/, "22" ~~ /(\d+)/);
my @caps = @ms.map({ $_[0] });
is @caps».fmt("%02d").join(","), "01,22", '».fmt on a list of Match objects coerces each';

# A List target's own `.fmt` (the joinable-list branch) must coerce each item too.
is (1, "255" ~~ /(\d+)/, 3).fmt("%d"), "1 255 3", 'List.fmt coerces a Match item';

# A Pair whose value is a Match (the pair branch).
my $p = Pair.new("k", "255" ~~ /(\d+)/);
is $p.fmt("%s:%d"), "k:255", 'Pair.fmt coerces a Match value';

# Plain numeric args are unaffected (no regression on the common fast path).
is sprintf("%d/%.1f", 10, 2.5), "10/2.5", 'plain numeric args unaffected';
