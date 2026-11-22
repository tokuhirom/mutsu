use Test;

# A `Date`/`DateTime` subclass, or a value with a role mixed in, that declares
# its own `ACCEPTS` is the matcher of `$topic ~~ $it`: its method wins over the
# `Date`/`DateTime` row it overrides. The smartmatch operator reached the row
# first and compared by identity (Chronic's `DateTimeMatcher`, whose
# `Chronic::Description.new ~~ $now` failed once `but` reblesses a DateTime
# into a real `DateTime+{R}` type). Expected values are rakudo 2026.11's.

plan 7;

class Desc { }
class MyDate is Date { multi method ACCEPTS(Desc $d) { True } }
class MyDateTime is DateTime { method ACCEPTS($d) { $d ~~ Desc } }
role MultiMatcher { multi method ACCEPTS(Desc $d) { True } }
role OnlyMatcher { method ACCEPTS($d) { $d ~~ Desc } }

ok Desc.new ~~ MyDate.new(2020, 1, 1), 'a Date subclass multi ACCEPTS';
ok Desc.new ~~ MyDateTime.new(2020, 1, 1, 0, 0, 0), 'a DateTime subclass ACCEPTS';
ok Desc.new ~~ (Date.new(2020, 1, 1) but MultiMatcher), 'a Date with a role multi ACCEPTS';
ok Desc.new ~~ (DateTime.new(2020, 1, 1, 0, 0, 0) but OnlyMatcher), 'a DateTime with a role ACCEPTS';
nok 42 ~~ (DateTime.new(2020, 1, 1, 0, 0, 0) but OnlyMatcher), '... which decides a non-match too';

# The row still answers what the user method does not declare.
my $d = Date.new(2020, 1, 1) but MultiMatcher;
is $d.day-of-week, 3, 'other Date methods still come from the row';
is (Desc.new, 1, Desc.new).grep($d).elems, 2, 'grep with the matcher';
