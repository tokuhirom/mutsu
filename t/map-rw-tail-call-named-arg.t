use Test;

# A bare tail `Stmt::Call` carrying named args (how an imported/module sub call
# like `f(k => v)` parses) used to be compiled as a value-discarding statement on
# the `@array.map` rw path (`eval_map_over_items_rw`), so the map result wrongly
# fell back to the topic `$_` instead of the call's return value. The non-rw path
# (`eval_map_over_items`) already normalized the tail; this test pins the rw path.

plan 6;

# A module-style sub returning a distinct type, called with a named arg as the
# block's tail statement. Mapping a *concrete @-array* (the rw path) must return
# the sub's value, not the topic.
sub wrap(:$content) returns Str { "wrapped:$content" }

my @items = 1, 2, 3;
my @mapped = @items.map: { wrap content => $_ };
is @mapped[0], "wrapped:1", "named-arg tail call on rw map returns sub value (1)";
is @mapped[1], "wrapped:2", "named-arg tail call on rw map returns sub value (2)";
is @mapped[2], "wrapped:3", "named-arg tail call on rw map returns sub value (3)";

# The same over a literal list (the non-rw path) already worked; keep it pinned.
my @lit = (10, 20).map: { wrap content => $_ };
is @lit[0], "wrapped:10", "named-arg tail call on list map returns sub value (1)";
is @lit[1], "wrapped:20", "named-arg tail call on list map returns sub value (2)";

# Positional tail call already worked; guard against regressions.
sub double($n) { $n * 2 }
my @pos = (1, 2, 3).map: { double $_ };
is @pos.join(","), "2,4,6", "positional tail call on map still works";
