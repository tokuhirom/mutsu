use Test;

# `class Foo is Date { multi method new(...) { self.Date::new(...) + ... } }`
# is the shape used by the `Date::YearDay` distribution (P5seek's sibling on
# CPAN Butterfly Plan): a `Date` subclass with its own multi `new` that calls
# the base type's constructor via a qualified call, then does Date arithmetic
# on the result. Two separate bugs used to break this:
#
#  1. `self.Date::new(...)` from a type object (before an instance exists)
#     fell back to unqualified `.new` dispatch, which re-entered the caller's
#     OWN overriding `new` instead of Date's native constructor logic —
#     "Default constructor for 'Foo' only takes named arguments". From an
#     instance it just failed outright — "No such method 'Date::new'".
#  2. Date `+`/`-` arithmetic recognized only the literal class name "Date"
#     as a valid operand, so even a correctly-built subclass instance was not
#     treated as date-like at all, and the result of `+`/`-` was always
#     rebuilt as a plain `Date`, discarding the subclass type and any custom
#     attributes (Rakudo's `Date::infix:<+>` is `self.clone(:days(...))`,
#     which preserves both).

plan 9;

class YearDay is Date {
    multi method new(:$year!, :$day-of-year!, |c) {
        self.Date::new($year - 1, 12, 31) + $day-of-year
    }
    multi method new(:$year!, :$doy!, |c) {
        self.Date::new($year - 1, 12, 31) + $doy
    }
}

my $d = YearDay.new(:year(2121), :day-of-year(42));
isa-ok $d, YearDay, 'qualified-new-then-+ result is the subclass, not Date';
isa-ok $d, Date, 'and still isa Date';
is $d, Date.new(2121, 2, 11), 'day-of-year 42 in 2121 lands on the correct date';

my $d2 = YearDay.new(:year(2121), :doy(42));
is $d2, $d, 'the :doy candidate agrees with the :day-of-year candidate';

# self.Date::new(...) called directly on an existing instance (not just a
# type object mid-construction).
{
    class Wrapper is Date {
        method rebuild { self.Date::new(self.year, self.month, self.day) }
    }
    my $w = Wrapper.new(2020, 6, 15).rebuild;
    isa-ok $w, Wrapper, 'self.Date::new on an instance also blesses as the subclass';
    is $w.Str, '2020-06-15', 'and carries the right date';
}

# Date subclass arithmetic keeps the subclass type and custom attributes.
{
    class Tagged is Date {
        has $.tag = 'default';
    }
    my $t = Tagged.new(2020, 1, 1, :tag<hello>);
    my $t2 = $t + 5;
    isa-ok $t2, Tagged, 'Date subclass + Int stays the subclass type';
    is $t2.tag, 'hello', 'and preserves a custom attribute (like .clone would)';
    is $t2.Str, '2020-01-06', 'with the correct resulting date';
}
