use Test;

# `>>.{...}` / `»{...}` hyper associative subscript (postcircumfix `{}` hypered).
# Regression: mutsu failed to parse `>>.{key}` at all, and hyper subscripts with
# a Range/list (slice) index returned Nil instead of slicing each element.
# Found via the real-distribution compat sweep (Taurus::CLI, docs/dist-compat-sweep.md).

plan 10;

my @h = {a => 1, b => 2}, {a => 3, b => 4};

# Single scalar key
is-deeply @h>>.{'a'}.List, (1, 3), 'hyper .{scalar} extracts one key per element';
is-deeply @h>>.<a>.List, (1, 3), 'hyper .<a> (angle) still works';

# Dotless form
is-deeply (@h>>{'b'}).List, (2, 4), 'hyper {scalar} (dotless) works';

# Range slice per element (the Taurus::CLI case)
my @rows = {0 => 'a', 1 => 'b', 2 => 'c', 3 => 'd'},
           {0 => 'A', 1 => 'B', 2 => 'C', 3 => 'D'};
is-deeply @rows>>.{0..2}, (('a', 'b', 'c'), ('A', 'B', 'C')), 'hyper .{range} slices each element';

# Multi-key list slice
is-deeply @h>>.{'a', 'b'}, ((1, 2), (3, 4)), 'hyper .{list} slices each element';

# Positional hyper slice (twin fix)
my @cells = [10, 20, 30, 40], [11, 21, 31, 41];
is-deeply @cells>>.[1..2], ((20, 30), (21, 31)), 'hyper .[range] slices each element';
is-deeply @cells>>.[0], (10, 11), 'hyper .[scalar] extracts one position per element';
is-deeply @cells>>.[0, 3], ((10, 40), (11, 41)), 'hyper .[list] slices each element';

# Hyper result of a nodal subscript is a List even over an Array target
my @arr = {x => 1}, {x => 2};
isa-ok (@arr>>.{'x'}), List, 'hyper subscript over Array yields a List';

# Guillemet form parses and slices
is-deeply (@rows>>.{1..2}), (('b', 'c'), ('B', 'C')), 'hyper .{range} via >> form';
