use v6;
use Test;

# `IterationBuffer.new(...)` flattens its arguments into the buffer's items —
# pure data — yet `.new` round-tripped through the interpreter's `dispatch_new`.
# It now builds through the shared `try_native_builtin_construct` entry via
# `build_native_iterationbuffer_value`, which the interpreter arm also calls.
#
# NOTE: raku's IterationBuffer.new only takes named args (a positional dies);
# mutsu's constructor is leniently positional. This test pins mutsu's
# (interpreter-matching, byte-identical) behavior — it is not run against raku.

plan 6;

is IterationBuffer.new(1, 2, 3).elems, 3, 'IterationBuffer.new from elements';
is IterationBuffer.new.elems, 0, 'empty IterationBuffer.new';
is IterationBuffer.new([10, 20], 30).elems, 3, 'an Array argument is spread';
my $c = IterationBuffer.new(1, 2);
$c.push(3);
is $c.elems, 3, 'push works on a natively-built buffer';
is IterationBuffer.new($c).elems, 3, 'another IterationBuffer is copied in';
is IterationBuffer.new.WHAT.^name, 'IterationBuffer', 'WHAT is IterationBuffer';
