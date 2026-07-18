use v6;
use lib 't/lib';
use Test;
use TailNamedCall;

plan 5;

# A statement-position call with NAMED arguments as the last statement of a
# body must yield the call's value (the body result). It previously compiled
# to a value-less statement op, so the routine returned its stale topic —
# JSON::Marshal's `marshal` (tail `to-json($ret, :$sorted-keys, :$pretty)`)
# returned Any and META6's `$m.to-json` died in the `--> Str` return check.

sub f(:$x) { "v" ~ $x }

my $r = do { f(:x(1)) };
is $r, "v1", 'do-block tail named call yields the value';

sub g(--> Str) { f(:x(2)) }
is g(), "v2", 'sub tail named call yields the value';

is render({b => 2}, :!pretty), Q<{"b":2}>, 'module sub with native tail named call returns its value';

my $r2 = do { render({c => 3}, :!pretty) };
is $r2, Q<{"c":3}>, 'do-block over the module sub keeps the value';

role R {
    method go(--> Str) { render(self.x, :!pretty) }
}
my class P does R { has $.x }
is P.new(x => {d => 4}).go(), Q<{"d":4}>, 'role-method caller gets the module sub value';

done-testing;
