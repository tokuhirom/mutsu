use v6;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;

# A `use` in a routine body re-imports on every call and rolls back when the
# call returns. The rollback judges only the keys the call's imports installed
# when nothing else wrote the routine table (#9170); these pin that it still
# takes every imported alias away, call after call, and that a body which also
# declares a routine takes the full path and is just as right.

sub uses-test(Int $n) { use Test; ok $n > 0, "import inside the call works ($n)"; $n }
sub uses-test-and-declares() {
    use Test;
    my sub helper { 7 }
    is helper(), 7, 'a body that also declares a routine imports too';
}

use Test;
plan 11;

for 1..3 -> $n { uses-test($n) }
uses-test-and-declares();
uses-test-and-declares();

ok (try EVAL '&is-approx').defined, 'the program\'s own `use Test` still answers after the calls';

sub no-import() { (try EVAL 'is-deeply-nope(1)') // 'gone' }
is no-import(), 'gone', 'an unknown routine is still unknown';

sub uses-fixture($x) { use RoutineScopedUseFixture; rsuf-greet() ~ ' ' ~ rsuf-m($x) }
is uses-fixture(1), 'hi int 1', 'a routine-scoped import of a sub and a multi works';
is uses-fixture('a'), 'hi str a', '... and again on the next call';
nok (try EVAL 'rsuf-greet()').defined, 'the imported sub is gone after the calls';
nok (try EVAL 'rsuf-m(1)').defined, 'the imported multi is gone after the calls';
