use v6;
use lib 't/lib';
use Test;
use DoForValueSub;

# A bare call to an imported routine as the last statement of a
# value-collecting body parses as a *statement* call (the parser knows the
# name is a routine once `use` has been processed). It must still yield its
# return value; it used to be compiled through the sink path, so the body
# collected Nil.

plan 9;

is-deeply (do for ^2 { plainsub('x') }).List, ("P:x", "P:x"),
    'do for collects an imported sub call';

is-deeply (do for ^2 -> $i { plainsub('x') }).List, ("P:x", "P:x"),
    'do for with an explicit loop parameter collects it too';

is-deeply (do for ^2 { plainsub('x'); }).List, ("P:x", "P:x"),
    'a trailing semicolon does not sink the value';

is-deeply (do for ^1 { plainsub('a'); plainsub('b') }).List, ("P:b",),
    'only the last statement is the value; earlier ones stay in sink context';

my @collected = do for 1..2 { plainsub('m') };
is-deeply @collected, ["P:m", "P:m"], 'assigning the collected list works';

is (do if 1 { plainsub('a') }), 'P:a', 'do if yields an imported sub call';

is (do given 1 { plainsub('g') }), 'P:g', 'do given yields an imported sub call';

# The named-argument shape goes through the ExecCallPairs keep_value path.
is-deeply (do for ^2 { namedsub('x', :upper) }).List, ("N:X", "N:X"),
    'a tail statement call with named args keeps its value';

# The parenthesized form always worked; keep it pinned as the control.
is-deeply (do for ^2 { (plainsub('x')) }).List, ("P:x", "P:x"),
    'the parenthesized form still works';
