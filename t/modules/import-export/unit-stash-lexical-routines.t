use lib 't/lib';
use Test;
use UnitStashExport;

# `UNIT::` is the compilation unit's outermost LEXICAL pad, not a package called
# "UNIT", and it must list the unit's own `my sub`s from inside a routine of that
# unit -- which is exactly where a hand-written `sub EXPORT` reads them.
#
# Two bugs met here before: `UNIT::` resolved as a package and answered an empty
# stash, and the lexical pseudo-stash enumerated only `our`/package routines, so
# the standard `UNIT::.grep: { .key.starts-with('&') }` export idiom (which
# `t/lib/UnitStashExport.rakumod` uses verbatim, as String::Utils does) exported
# nothing at all. The second one was also ORDER-DEPENDENT -- a routine appeared
# only once something had already called through its name -- which is why the
# repeated reads below are part of the test.

plan 7;

my sub local-helper() { 42 }

sub probe() {
    UNIT::.keys.grep(*.starts-with('&')).sort.join(",")
}

ok probe().contains('&local-helper'),
  'UNIT:: lists a unit-level `my sub` from inside a routine';
is probe(), probe(),
  'UNIT:: answers the same set on a second read (no call-warmed entries)';
ok probe().contains('&probe'),
  'a plain file-scope `sub` (also lexical) is listed too';

isa-ok UNIT::, PseudoStash, 'UNIT:: is a PseudoStash, not a package stash';
ok UNIT::.elems > 0, 'UNIT:: is not empty';

is UNIT::<&local-helper>(), 42,
  'the routine UNIT:: lists is the callable one';

is-deeply (alpha(), beta()), ("A", "B"),
  'a module whose sub EXPORT enumerates UNIT:: exports its lexical subs';

# vim: expandtab shiftwidth=4
