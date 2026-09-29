use Test;

# From the Date::WorkdayCalendar distribution: a `class Workdate is Date`.

plan 8;

my $d = Date.new('2011-12-01');
is $d.Date::succ, '2011-12-02', 'qualified call to a builtin method on a plain Date';
is $d.Date::pred, '2011-11-30', 'qualified pred on a plain Date';

class W is Date {
    has $.cal is rw;
    multi method new(W: :$year!, :$month, :$day) { self.Date::new(:$year, :$month, :$day) }
    multi method new(W: Str:D $s, $c?) { self.Date::new($s) }
    method succ() { 42 }
}

my $w = W.new('2011-12-01');
is $w.succ, 42, 'the subclass override wins for the unqualified call';
is $w.Date::succ, '2011-12-02', 'qualified call reaches the builtin, not the override';

my $from-date = W.new($d);
is $from-date.WHAT.^name, 'W', 'inherited Date.new candidate: result is blessed as the subclass';
is $from-date.Str, '2011-12-01', 'inherited Date.new candidate keeps the date';
is W.new(2011, 1, 2).Str, '2011-01-02', 'positional year/month/day reaches Date.new';
is W.new(year => 2011, month => 1, day => 2).Str, '2011-01-02', 'own named candidate still works';
