use Test;

# rakudo's last-resort candidate for `==` is
# `multi infix:<==>(Any \a, Any \b) { a.Numeric == b.Numeric }`, so two objects
# compare by their `.Numeric` even when neither does `Real` or `Numeric`.
# `DateTime` is exactly that shape.

plan 10;

my $utc = DateTime.new('2016-12-31T23:59:60Z');

nok $utc ~~ Real,    'DateTime does not do Real (same as rakudo)';
nok $utc ~~ Numeric, 'DateTime does not do Numeric (same as rakudo)';

# The same instant rendered in a different timezone is a DIFFERENT object with
# different attributes, so structural equality is the wrong answer.
ok $utc.in-timezone(7200) == $utc,
    'two DateTimes naming the same instant compare equal across timezones';
ok $utc.in-timezone(-7200) == $utc, '... and with a negative offset';
nok $utc.in-timezone(7200) != $utc, '`!=` agrees with `==`';

# The routine form must answer identically -- it is how `cmp-ok` reaches the
# operator, and it is the only form the vendored Test.rakumod ever uses.
ok &infix:<==>($utc.in-timezone(7200), $utc),
    'the routine form of `==` agrees';

# Two genuinely different instants stay unequal.
nok DateTime.new(2016,1,1,0,0,0) == DateTime.new(2016,1,1,0,0,1),
    'different instants are not equal';

# The temporal operators that have their own candidates must NOT be numified
# away by this fallback.
my $a = DateTime.new(2016,1,1,0,0,0);
my $b = DateTime.new(2016,1,1,0,0,10);
isa-ok $b - $a, Duration, 'DateTime - DateTime is still a Duration';
is ($a <=> $b), Order::Less, 'DateTime <=> DateTime still orders';

# A user class with its own `.Numeric` goes down the same road, and two
# structurally different objects that numify alike compare equal.
# (mutsu is more lenient than rakudo for an object with NO `.Numeric` at all:
# rakudo dies with `Cannot resolve caller Numeric(...)`, mutsu answers False.
# That predates this change and is tracked in
# todo/tickets/numeric-op-on-an-object-without-numeric-answers-instead-of-dying.md.)
class Weight { has $.kg; has $.label; method Numeric { $!kg } }
ok Weight.new(kg => 3, label => 'a') == Weight.new(kg => 3, label => 'b'),
    'two objects that numify alike are equal even with different attributes';
