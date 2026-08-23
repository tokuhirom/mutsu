use v6;
use Test;

plan 21;

# A Backtrace is Positional over its frames: `$bt[N]` reads the
# Backtrace::Frame at that position, `[*-1]` the last one, and an
# out-of-range index reads back as Nil (rakudo keeps the frames in a
# List, not an Array). Regression pin for the bug where every positional
# index into a Backtrace returned Nil/Any regardless of the index.

sub zipi { { { die "Something bad happened" }() }() };
try {
    zipi;
}

my $bt = $!.backtrace;
ok $bt.defined, 'a caught exception carries a Backtrace';
ok $bt.elems > 0, 'the Backtrace has at least one frame';

my $first = $bt[0];
isa-ok $first, Backtrace::Frame, '$bt[0] is a Backtrace::Frame';
ok $first.file.chars > 0, '$bt[0].file is set';
ok $first.line > 0, '$bt[0].line is set';

my $last = $bt[*-1];
isa-ok $last, Backtrace::Frame, '$bt[*-1] is a Backtrace::Frame';
ok $last.file.chars > 0, '$bt[*-1].file is set';

is $bt[$bt.elems - 1].line, $last.line, '[*-1] is the same frame as [elems-1]';
is $bt[*-1].subname, $bt.list[*-1].subname, '[*-1] agrees with .list[*-1]';
is $bt[0].subname, $bt.list[0].subname, '[0] agrees with .list[0]';

# Middle index, when there is one.
if $bt.elems > 2 {
    isa-ok $bt[1], Backtrace::Frame, '$bt[1] is a Backtrace::Frame';
} else {
    skip 'backtrace too short for a middle index', 1;
}

# Out of range reads back as Nil, exactly as it does off a List.
nok $bt[$bt.elems].defined, 'an index one past the end is undefined';
is $bt[1000].raku, Nil.raku, 'a far out-of-range index is Nil';

# Every index shape a List supports works through the Backtrace too.
is $bt[0, 1].elems, 2, 'a two-element slice yields two frames';
is $bt[^2].elems, 2, 'a range slice yields two frames';
is $bt[0 .. *-1].elems, $bt.elems, 'a [0 .. *-1] slice covers every frame';
is $bt[*].elems, $bt.elems, 'a whatever slice covers every frame';
is-deeply $bt[0, 1].map(*.line).List, $bt.list[0, 1].map(*.line).List,
        'slice frames agree with .list';

# The explicit AT-POS spelling matches the subscript.
is $bt.AT-POS(0).line, $bt[0].line, '.AT-POS(0) matches $bt[0]';
nok $bt.AT-POS($bt.elems).defined, '.AT-POS past the end is undefined';

# An associative subscript is a different question and stays undefined.
nok $bt<nosuchkey>.defined, 'an associative subscript on a Backtrace is undefined';

