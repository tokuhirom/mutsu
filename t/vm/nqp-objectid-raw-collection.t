use Test;
use nqp;

# `nqp::objectid` and `.WHERE` answer for a raw `BOOTHash` / `BOOTArray` too
# (`.WHERE` is a pseudo-method every object has). EEL's state copier keys
# shared capture storage by `nqp::objectid` of a Capture's raw `%!hash` (#16888).

plan 6;

my $h := nqp::hash;
ok nqp::objectid($h) == nqp::objectid($h), 'a raw hash has a stable objectid';
ok nqp::objectid($h) != nqp::objectid(nqp::hash), 'distinct raw hashes differ';
ok $h.WHERE == $h.WHERE, '.WHERE on a raw hash';
my $l := nqp::list;
ok nqp::objectid($l) == nqp::objectid($l), 'a raw list has a stable objectid';
ok nqp::objectid($l) != nqp::objectid(nqp::list), 'distinct raw lists differ';
my %m;
my $s := nqp::getattr(%m, Map, '$!storage');
ok nqp::objectid($s) == nqp::objectid(nqp::getattr(%m, Map, '$!storage')),
    'the same storage read twice is one object';
