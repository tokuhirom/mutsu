use Test;

# A `die` captures its call stack at the throw but renders the backtrace only
# when something reads it (#9172). Whatever runs between the throw and the
# read -- deeper calls, other throws from other frames -- must not leak into
# the backtrace that is finally rendered.
#
# All expectations below were measured against rakudo v2026.07.

plan 10;

sub names($e) { $e.backtrace.list.map(*.subname).grep(* ne '').list }

sub inner-a { die "a" }
sub outer-a { inner-a() }
sub inner-b { die "b" }
sub outer-b { inner-b() }

my ($ea, $eb);
try { outer-a(); CATCH { default { $ea = $_ } } }
try { outer-b(); CATCH { default { $eb = $_ } } }

# Read the first backtrace only after the second throw reused its depth.
ok names($ea).grep('inner-a'), 'the first backtrace names its own thrower';
nok names($ea).grep('inner-b'), 'and not the frame a later throw came from';
ok names($eb).grep('inner-b'), 'the second backtrace names its own thrower';
nok names($eb).grep('inner-a'), 'and not the earlier one';

# Read a backtrace from deeper in the stack than it was thrown.
sub deep($n) { $n ?? deep($n - 1) !! $ea.backtrace.Str }
my $text = deep(5);
ok $text.contains('inner-a'), 'rendered deeper down, the text keeps the throw site';
nok $text.contains('deep'), 'and does not show the frames of the reader';

# The line of each frame is the line at the throw.
my $line = $ea.backtrace.list.first(*.subname eq 'inner-a').line;
is $line, 14, 'the throwing frame reports the line of the die';

# Throws in a loop at one depth each keep their own backtrace.
my @es;
for ^3 { try { outer-a(); CATCH { default { @es.push($_) } } } }
is @es.elems, 3, 'three exceptions caught';
is-deeply @es.map({ names($_).grep('inner-a').elems }).list, (1, 1, 1),
    'each names the thrower once';

# `$!` of a plain try keeps a readable backtrace too.
try { outer-b() }
ok $!.backtrace.Str.contains('outer-b'), '$! renders its backtrace on demand';
