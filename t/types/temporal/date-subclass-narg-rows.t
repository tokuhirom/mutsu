use Test;

# `Date`'s and `DateTime`'s methods that take arguments are rows; an instance of
# a subclass reaches them through their owner and keeps its class
# (ADR-11276 §9.127).

plan 22;

class D is Date {}
class DT is DateTime {}

my $d  = D.new(2024, 2, 10);
my $t  = DT.new(2024, 2, 10, 3, 4, 5.5, :timezone(3600));
my $bt = DateTime.new(2024, 2, 10, 3, 4, 5.5, :timezone(3600));

# Date subclass.
isa-ok $d.clone(:year(2000)), D, 'clone(:year) keeps the subclass';
is $d.clone(:day(3)).Str, '2024-02-03', 'clone(:day)';
is $d.later((:2days, :1month)).Str, '2024-03-12', 'later with a list of pairs';
isa-ok $d.earlier((:2days,)), D, 'earlier with a list keeps the subclass';
is $d.yyyy-mm-dd('/'), '2024/02/10', 'yyyy-mm-dd($sep)';
is $d.dd-mm-yyyy('.'), '10.02.2024', 'dd-mm-yyyy($sep)';
throws-like { Date.new(2024, 1, 1).in-timezone(0) }, X::Method::NotFound,
    'Date has no in-timezone';
throws-like { $d.in-timezone(0) }, X::Method::NotFound,
    'nor has a Date subclass';

# DateTime subclass.
isa-ok $t.clone(:year(2000)), DT, 'DateTime clone keeps the subclass';
is $t.clone(:hour(7), :timezone(0)).Str, '2024-02-10T07:04:05.500000Z', 'clone(:hour, :timezone)';
isa-ok $t.utc, DT, 'utc keeps the subclass';
is $t.utc.hour, 2, 'utc moves the hour';
isa-ok $t.later((:2hours, :30minutes)), DT, 'later with a list keeps the subclass';
is $t.later((:2hours, :30minutes)).minute, 34, 'later with a list applies the units';
is $t.yyyy-mm-dd('/'), '2024/02/10', 'DateTime yyyy-mm-dd($sep)';

# posix($ignore-timezone?, :$real), on the class and a subclass.
for $bt, $t -> $x {
    my $n = $x.^name;
    is $x.posix(True), 1707534245, "$n posix(True) ignores the timezone";
    is $x.posix(:real), 1707530645.5, "$n posix(:real) is the fraction as a Rat";
    is $x.posix(True, :real), 1707534245.5, "$n posix(True, :real)";
}
isa-ok $bt.posix(:real), Rat, 'posix(:real) is a Rat';
