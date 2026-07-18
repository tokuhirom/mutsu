use v6;
use Test;

# MONKEY-TYPING on builtin native classes (JSON::Fast t/07-datetime.t parity):
# an augmented `multi method new` candidate wins when it matches, and falls
# back to the native constructor otherwise.

use MONKEY-TYPING;
augment class DateTime { multi method new(Any:U) { $?CLASS } }
augment class Date { multi method new(Any:U) { $?CLASS } }

ok DateTime.new(Any) === DateTime, 'augmented DateTime.new(Any:U) candidate matches';
ok Date.new(Any) === Date, 'augmented Date.new(Any:U) candidate matches';

my $dt = DateTime.new(2026, 7, 19, 12, 34, 56);
is $dt.year, 2026, 'native DateTime.new still works after augment';
is Date.new("2026-07-19").day, 19, 'native Date.new still works after augment';
dies-ok { DateTime.new }, 'no-arg DateTime.new still rejected';

# Instant serializes as its .DateTime ISO string in to-json.
{
    use JSON::Fast;
    my $i = now;
    my $json = to-json([$i], :!pretty);
    like $json, /'["' \d**4 '-' \d\d '-' \d\d 'T'/, 'Instant serializes as ISO datetime';
    my $trip = DateTime.new(from-json($json)[0]).Instant;
    ok $trip.to-posix[0] - $i.to-posix[0] < 1, 'Instant roundtrips through its DateTime string';
}

# Hyper forms of the comparison ops ending in `=` are not `op=` assignments.
is-deeply ((1.0, 2.0) »=~=« (1.0, 2.0)).List, (True, True), 'hyper =~= works';
is-deeply ((1, 2) »=:=« (1, 2)).List, (True, True), 'hyper =:= works';

done-testing;
