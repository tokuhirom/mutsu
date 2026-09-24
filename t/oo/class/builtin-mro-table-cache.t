use Test;

# Pins the MRO answers of the hardcoded builtin types (`Match`, `Capture`,
# `IO::Spec::*`) across repeated method calls. Their interned chain is
# memoized per thread (issue #8888); every call after the first reads the
# memo, so a wrong memo key shows up as a wrong answer on a later call, not
# the first one.

plan 12;

"abc def" ~~ /(\w+) \s (\w+)/;

my $sum = 0;
for ^50 { $sum += $/.from + $/.to + $0.Str.chars + $1.chars }
is $sum, 50 * (0 + 7 + 3 + 3), 'Match accessors answer the same on every call';

is Match.^mro.map(*.^name).join(','), 'Match,Capture,Cool,Any,Mu', 'Match MRO';
is Capture.^mro.map(*.^name).join(','), 'Capture,Any,Mu', 'Capture MRO';
is IO::Spec::Unix.^mro.map(*.^name).join(','), 'IO::Spec::Unix,IO::Spec,Any,Mu',
    'IO::Spec::Unix MRO';
is IO::Spec::Win32.^mro.map(*.^name).join(','),
    'IO::Spec::Win32,IO::Spec::Unix,IO::Spec,Any,Mu',
    'IO::Spec::Win32 MRO is not confused with the Unix one after both are asked';

# Asking one table entry must not hand its chain to another: interleave them.
for ^3 {
    Match.^mro;
    Capture.^mro;
}
is Capture.^mro.elems, 3, 'Capture MRO length after interleaved lookups';
is Match.^mro.elems, 5, 'Match MRO length after interleaved lookups';

ok $/ ~~ Capture, 'a Match is a Capture';
ok $/ ~~ Cool, 'a Match is Cool';
nok Capture.new ~~ Cool, 'a Capture is not Cool';

# A user method on a Match subclass still wins over the native one, on every
# call (the has_user_method gate walks the same chain).
class MyMatch is Match { method chars() { 42 } }
my $m = MyMatch.new;
is (^3).map({ $m.chars }).join(','), '42,42,42', 'subclass override wins every time';

# A role composed onto a grammar still overrides a native entry point.
role R { method parse($s) { "role parse $s" } }
grammar G does R { token TOP { x } }
is G.parse('x'), 'role parse x', 'role method on a grammar wins over native parse';
