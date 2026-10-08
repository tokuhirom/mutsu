use Test;

plan 35;

# Backtrace's and Backtrace::Frame's methods are built-in method rows
# (ADR-11276 §9.35). The expectations are relations between the answers (the
# frame count differs between Rakudo and mutsu), each checked against Rakudo.
sub inner { die "boom" }
sub middle { { inner() }() }
sub outer { middle() }

my $bt;
try { outer(); CATCH { default { $bt = .backtrace } } }

isa-ok $bt, Backtrace, 'the exception carries a Backtrace';
ok $bt.elems > 1, 'several frames';
is $bt.gist, "Backtrace({$bt.elems} frames)", 'gist counts the frames';
is $bt.list.elems, $bt.elems, 'list';
is $bt.flat.elems, $bt.elems, 'flat';
is-deeply $bt.is-runtime, True, 'is-runtime';
is $bt.full.lines.elems, $bt.elems, 'full is one line per frame';
ok $bt.concise.lines.elems <= $bt.full.lines.elems, 'concise keeps no more than full';
ok $bt.summary.lines.elems <= $bt.full.lines.elems, 'summary keeps no more than full';
ok $bt.nice.lines.elems <= $bt.full.lines.elems, 'nice keeps no more than full';
is $bt.nice(:oneline).lines.elems, 1, 'nice(:oneline) is one line';
ok $bt.Str.chars > 0, 'Str is the rendered text';

# AT-POS and the frames
isa-ok $bt[0], Backtrace::Frame, 'the subscript reads a frame';
isa-ok $bt.AT-POS(0), Backtrace::Frame, 'AT-POS';
is-deeply $bt.AT-POS($bt.elems + 10), Nil, 'AT-POS out of range is Nil';

# next-interesting-index / outer-caller-idx
my $next = $bt.next-interesting-index;
ok $next ~~ Int, 'next-interesting-index answers an index';
ok $bt.next-interesting-index(0) ~~ Int, 'next-interesting-index with a start index';
ok $bt.next-interesting-index(0, :named) ~~ Int, 'next-interesting-index with a flag';
is-deeply $bt.next-interesting-index($bt.elems + 5), Nil, 'past the end is Nil';
isa-ok $bt.outer-caller-idx(0), Array, 'outer-caller-idx answers an Array';
is-deeply $bt.outer-caller-idx(-1), [], 'a negative start has no caller';

# the frame
# frame 0 is the setting frame of `die`; the first user frame is `inner`.
my $f = $bt.list.first({ !.is-setting });
is $f.subname, 'inner', 'subname of the innermost frame';
ok $f.file ~~ Str && $f.file.chars > 0, 'file';
ok $f.line ~~ Int && $f.line > 0, 'line';
is $f.code.name, 'inner', 'code.name';
ok $f.Str.contains('inner'), 'Str names the sub';
ok $f.Str.ends-with("\n"), 'Str is newline-terminated';
is-deeply $f.is-routine, True, 'a named sub is a routine';
is-deeply $f.is-hidden, False, 'is-hidden';
is-deeply $f.is-setting, False, 'is-setting';
is-deeply $f.subname, $f.code.name, 'subname agrees with code.name';

# an anonymous block frame is not a routine
my $block = $bt.list.first({ .subname eq '' });
ok $block.defined, 'the block frame is there';
is-deeply $block.is-routine, False, 'an anonymous block is not a routine';

# the rows answer the same through a variable and inline
is $bt.elems, $bt.list.elems, 'a variable receiver';
is Backtrace.new.elems, Backtrace.new.list.elems, 'an inline receiver';
