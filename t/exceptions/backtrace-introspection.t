use Test;

plan 34;

# `Backtrace.full` renders one frame per line, and the three introspection
# methods `next-interesting-index`, `outer-caller-idx` and `nice`.
#
# Every assertion here is *relative* to the backtrace's own frame list: mutsu
# models no CORE-setting frames (its `die`/`throw` are native, not Raku
# routines with callframes), so a given index names a different frame than it
# does in Rakudo and the absolute counts differ. See
# `todo/tickets/backtrace-frame-indexing-returns-nil.md`. This file therefore
# passes verbatim under both `raku` and `mutsu`.

sub inner  { die "boom" }
sub outer1 { inner()  }
sub outer2 { outer1() }
try outer2();

my $bt = $!.backtrace;

# ---------------------------------------------------------------- .full ----

ok $bt.elems > 1, 'the backtrace has more than one frame';
ok $bt.full.chars > 0, '.full is not empty';
ok $bt.full.lines.elems > 1, '.full is split across lines, not one long line';
is $bt.full.lines.elems, $bt.elems, '.full renders exactly one line per frame';
ok $bt.full.ends-with("\n"), '.full is newline-terminated';
nok $bt.full.lines.grep({ $_ !~~ /^ '  in ' \N+ ' at ' \N+ ' line ' \d+ $/ }),
    'every .full line has the "  in ... at FILE line N" shape';

# A frame's own .Str is one such newline-terminated line, and .full is just
# those strings concatenated -- which is what puts each frame on its own line.
is $bt.list[0].Str.lines.elems, 1, 'a frame stringifies to a single line';
ok $bt.list[0].Str.ends-with("\n"), 'a frame .Str is newline-terminated';
is $bt.full, $bt.list.map(*.Str).join, '.full is the concatenation of frame .Str values';

# The documented greps still hold, and are line-oriented for the same reason.
is $bt.concise, $bt.grep({ !.is-hidden && .is-routine && !.is-setting }).join,
    '.concise matches its documented grep';
is $bt.summary, $bt.grep({ !.is-hidden && (.is-routine || !.is-setting) }).join,
    '.summary matches its documented grep';
nok $bt.concise.lines.grep({ !$bt.full.contains($_) }),
    'every .concise line also appears in .full';

# ---------------------------------------- next-interesting-index ----------

my $first = $bt.next-interesting-index;
ok $first.defined, 'next-interesting-index finds an interesting frame';
ok 0 <= $first < $bt.elems, 'next-interesting-index returns a valid frame index';
is $first, $bt.next-interesting-index(0), 'the starting index defaults to 0';
nok $bt[$first].is-hidden, 'the interesting frame is not hidden';
nok $bt[$first].is-setting, 'the interesting frame is not a setting frame';

ok $bt.next-interesting-index($first) > $first,
    'passing an index scans strictly forward from it';

my $named = $bt.next-interesting-index(:named);
ok $named.defined, 'next-interesting-index(:named) finds a frame';
ok $bt[$named].subname.chars > 0, ':named lands on a frame that has a name';
ok $named >= $first, ':named never lands before the unfiltered answer';

nok $bt.next-interesting-index($bt.elems - 1).defined,
    'no interesting frame past the last one';
nok $bt.next-interesting-index($bt.elems + 100).defined,
    'an out-of-range start index answers Nil';

# ---------------------------------------------- outer-caller-idx ----------

my $o1 = $bt.list.first({ .subname eq 'outer1' }, :k);
ok $o1.defined, 'the backtrace contains a frame for outer1';

my @callers = $bt.outer-caller-idx($o1);
ok @callers ~~ Positional, 'outer-caller-idx returns a list of indices';
nok @callers.grep({ !($_ ~~ Int) || !($o1 < $_ < $bt.elems) }),
    'every caller index is a valid frame index below the given one';
nok @callers.grep({ !($bt[$_] ~~ Backtrace::Frame) }),
    'every caller index names a real frame';
is-deeply @callers.List, ($bt.elems - 1,).List,
    "a top-level sub's enclosing scope is the outermost <unit> frame";
is $bt.outer-caller-idx($bt.elems - 1).elems, 0,
    'the outermost frame has no outer caller';

# ------------------------------------------------------------- .nice ----

ok $bt.nice ~~ Str, '.nice returns a Str';
ok $bt.nice.ends-with("\n"), '.nice is newline-terminated';
ok $bt.nice.contains('in sub inner')
    && $bt.nice.contains('in sub outer1')
    && $bt.nice.contains('in block <unit>'),
    '.nice names the interesting frames';

is $bt.nice(:oneline).lines.elems, 1, '.nice(:oneline) is a single line';
ok $bt.nice(:oneline).contains('in sub outer1'),
    '.nice(:oneline) names the routine the innermost frame was called from';
