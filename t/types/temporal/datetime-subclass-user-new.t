use Test;

# From Games::TauStation::DateTime: a DateTime subclass with its own `new`.
plan 13;

my $f = my method { "custom" };
class G is DateTime {
    proto method new (|) {*}
    multi method new (|c) { self.DateTime::new: |c, :formatter($f) }
    multi method new (Str:D $_) { nextsame }
}

# `.now` calls `self.new(now, :$timezone, :&formatter)`
my $n = G.now;
isa-ok $n, G, '.now on a subclass with its own new builds the subclass';
ok $n.Instant.Rat > 1_000_000_000, '.now carries the current instant';
is $n.Str, 'custom', '.now goes through the user new (formatter applied)';

# qualified `self.DateTime::later` keeps the subclass
class H is DateTime {
    multi method later (|c) { self.DateTime::later: |c }
}
my $h = H.new('2020-01-01T00:00:00Z');
isa-ok $h.later(:5days), H, 'self.DateTime::later returns the subclass';
isa-ok $h.later(:5days).later(:2hours), H, 'and it can be chained';
is $h.later(:5days).Str, '2020-01-06T00:00:00Z', 'qualified later value';

# subtraction of subclass instances is Duration-valued
my $a = G.new('2018-04-23T00:57:13.361615Z');
my $b = DateTime.new('1964-01-22T00:00:27.689615Z');
isa-ok $a - $b, Duration, 'subclass - DateTime is a Duration';
is ($a - $b).Rat, 1712019432.672, 'including the leap seconds between them';

# :formatter(Callable) resets the formatter on clone
is $a.clone(:formatter(DateTime.now.formatter)).Str, '2018-04-23T00:57:13.361615Z',
    'clone with a type-object formatter resets to the default';
is $a.clone.Str, 'custom', 'plain clone keeps the formatter';

# pre-1970 Instants with a fractional part
is DateTime.new(Instant.from-posix(-4058280.5)).Str, '1969-11-15T00:41:59.500000Z',
    'negative fractional instant floors, not truncates';

# minutes and hours move the wall clock; a leap second is not counted
my $d = DateTime.new('2017-01-01T00:00:00Z');
is $d.earlier(:hours(1)).Str, '2016-12-31T23:00:00Z', 'earlier hours is wall-clock';
is $d.earlier(:seconds(3600)).Str, '2016-12-31T23:00:01Z', 'earlier seconds counts the leap second';

done-testing;
