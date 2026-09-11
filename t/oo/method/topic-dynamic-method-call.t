use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): an
# indirect method name held in a variable, called on the TOPIC — `.$name`. The
# explicit-invocant form (`$x.$name`) and the code-sigil topic form (`.&name`)
# each had their own parser branch, but the `$`/`@`/`%` topic form had none, so
# `@iterable.map({.$function})` (python::itertools) was a hard parse error.

plan 10;

# The construct the index reduced to.
my $double = { $_ * 2 };
is [1, 2].map({ .$double }), (2, 4), '.$code calls the code object on the topic';

# The same shapes the explicit-invocant form supports.
$_ = 3;
is (.$double), 6, '.$code works against a bare topic';
is 3.$double, 6, 'and the explicit-invocant form still does';

my $add = sub ($x, $y) { $x + $y };
is (.$add(4)), 7, '.$code(args) passes the topic as the first argument';

# A method captured as a Callable is invoked with the topic as its invocant.
my $upper = Str.^lookup('uc');
$_ = 'aBc';
is (.$upper), 'ABC', '.$code invokes a captured method on the topic';
is 'aBc'.$upper, 'ABC', 'and the explicit-invocant form agrees';

# `for` supplies the topic per iteration.
my @seen;
for 1, 2, 3 { @seen.push: .$double }
is @seen, [2, 4, 6], '.$code re-reads the topic on each iteration';

# A non-Callable is a runtime error, not a method name — `.$name` is an indirect
# INVOCATION, never a lookup by string.
my $not-code = 'uc';
dies-ok { $_ = 'aBc'; .$not-code }, '.$str does not name a method to call';

# `.&name` keeps its own meaning — a sub called with the topic as first argument.
sub triple($n) { $n * 3 }
is [1, 2].map({ .&triple }), (3, 6), '.&name still calls a named sub on the topic';

# A `.` immediately before a `$` inside a string is still literal text, not a
# method call — the interpolation parser owns that, and must keep owning it.
my $x = '5';
is "$x.$x", '5.5', 'a `.$var` inside a string interpolates two variables';
