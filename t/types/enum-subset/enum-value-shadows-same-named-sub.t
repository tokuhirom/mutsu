use Test;

# A bare identifier names a declared enum value before it names a same-named
# sub: the enum value is a term, the sub is `&name`. Parentheses still call
# the sub. Found via the TimeUnit distribution, which declares both
# `enum UnitTimeName (minutes => ...)` and `sub minutes(Numeric() $n)` and
# passes the enum value as `timeunit(3, minutes)`.

plan 7;

enum Unit (secs => 1, mins => 60);
sub mins(Numeric() $n) { $n * 60 }
sub secs() { 'the sub' }

is mins, Unit::mins, 'a bare name is the enum value';
is mins.value, 60, 'the enum value keeps its value';
is mins(2), 120, 'a call with parentheses reaches the sub';
is-deeply (3, mins).list, (3, Unit::mins).list, 'the enum value in a list';
is secs, Unit::secs, 'a zero-parameter sub does not shadow the enum value';
is secs(), 'the sub', 'empty parentheses still call the sub';

sub takes(Numeric $n, Unit $u) { $n * $u.value }
is takes(3, mins), 180, 'the enum value as an argument';
