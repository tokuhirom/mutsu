use Test;

# A `.wrap` of a built-in class method found with `.^lookup` reaches calls on
# the type object (#13626): BDD::Behave freezes the clock by wrapping
# `DateTime.^lookup('now')` and `Date.^lookup('today')`.

plan 8;

ok DateTime.^lookup('now') ~~ Method, 'DateTime.^lookup(now) is a Method';
ok Date.^lookup('today') ~~ Method, 'Date.^lookup(today) is a Method';
is DateTime.^can('now').elems, 1, 'DateTime.^can(now)';

my $frozen = DateTime.new(2001, 2, 3, 4, 5, 6);
my $handle = DateTime.^lookup('now').wrap(-> $type, *%named { $frozen });
is DateTime.now, $frozen, 'a wrapped DateTime.now answers the wrapper';
my $dt = DateTime;
is $dt.now, $frozen, 'through a variable holding the type object';
$handle.restore;
isnt DateTime.now, $frozen, 'restore unwraps it';

my $today = Date.^lookup('today').wrap(-> $type, *%named { 'day ' ~ callsame() });
like Date.today.Str, /^ 'day ' \d+ '-' /, 'callsame reaches the original Date.today';
$today.restore;

Int.^lookup('abs').wrap(-> $x { 'abs ' ~ callsame() });
is (-3).abs, 'abs 3', 'an instance method of a built-in class';
