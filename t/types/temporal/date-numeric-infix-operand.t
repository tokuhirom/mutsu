use Test;

# A Date operand of a numeric infix numifies to its day count, and a
# built-in class's `Real`-ness (Duration, Instant) still picks the Real
# bridge. mutsu answers both from the class (the native `Numeric` row, the
# type catalog) instead of re-running a full type check per call.

plan 14;

my $a = Date.new('2026-01-31');
my $b = Date.new('2026-02-01');
nok $a == $b, 'two Dates compare by day count';
ok $a == Date.new('2026-01-31'), 'equal Dates are ==';
ok $a < $b, 'and order by it';
is $a <=> $b, Less, '<=> on Dates';
ok $a == 61071, 'a Date equals its day count';
is $b - $a, 1, 'Date - Date is a day difference';
is ~($a + 1), '2026-02-01', 'Date + Int is a Date';

my $d = Duration.new(5);
is ($d + 1).raku, 'Duration.new(6.0)', 'Duration + Int stays a Duration';
is ($d * 1/2).raku, '2.5e0', 'Duration * Rat bridges through Real';
ok $d == 5, 'a Duration equals its seconds';
is (1/2 + $d).raku, 'Duration.new(5.5)', 'Rat + Duration';
is (Instant.from-posix(10) - 1).^name, 'Instant', 'Instant - Int is an Instant';

class R does Real { method Bridge { 2.5e0 } }
is R.new + 1, 3.5, 'a user Real still bridges';
is 1/2 + R.new, 3, 'on either side';
