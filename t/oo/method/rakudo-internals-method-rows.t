use v6;
use nqp;
use Test;

# `Rakudo::Internals` and `Rakudo::Sorting` answer their type-object methods
# from rows of the method table (ADR-11276, slice 5).

plan 9;

is Rakudo::Internals.IS-WIN, False, 'IS-WIN is False on this host';
is Rakudo::Internals.IS-WIN.WHAT.gist, '(Bool)', 'IS-WIN is a Bool';
isa-ok Rakudo::Internals.INITTIME, Num, 'INITTIME is a Num';
ok Rakudo::Internals.INITTIME > 0, 'INITTIME is after the epoch';
isa-ok Rakudo::Internals.INCLUDE, List, 'INCLUDE is a List';

my $keys := nqp::list_s("b", "a", "C");
my $sorted := Rakudo::Sorting.MERGESORT-str($keys);
is nqp::elems($sorted), 3, 'MERGESORT-str keeps every element';
is nqp::shift_s($sorted), 'C', 'and sorts by codepoint, so C leads';

throws-like { Rakudo::Internals.NO-SUCH-METHOD }, X::Method::NotFound,
    'an unlisted name is still a missing method';
throws-like { Rakudo::Internals.IS-WIN(1) }, X::AdHoc,
    'a surplus positional is refused';
