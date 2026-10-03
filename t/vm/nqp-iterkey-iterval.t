use v6;
use Test;
use nqp;

# `nqp::iterator`, `nqp::iterkey_s` and `nqp::iterval` (#11494) used to die
# with "Unsupported nqp:: op". Expected behaviour is rakudo's (MoarVM's):
# the iterator is truthy while elements remain; `nqp::shift` answers the next
# element of a list iterator, and the iterator itself (positioned on the next
# pair) for a hash iterator.

plan 6;

subtest 'iterating a Hash through its storage', {
    plan 2;
    my %h = a => 1, b => 2, c => 3;
    my $it := nqp::iterator(nqp::getattr(%h, Map, q/$!storage/));
    my %seen;
    while $it {
        my $e := nqp::shift($it);
        %seen{nqp::iterkey_s($e)} = nqp::iterval($e);
    }
    is-deeply %seen, %(a => 1, b => 2, c => 3), 'every pair is visited once';
    nok $it, 'the exhausted iterator is false';
}

subtest 'iterating an nqp::hash', {
    plan 3;
    my $it := nqp::iterator(nqp::hash('x', 5));
    ok nqp::istrue($it), 'a non-empty iterator is true';
    nqp::shift($it);
    is nqp::iterkey_s($it), 'x', 'iterkey_s reads the current key off the iterator';
    is nqp::iterval($it), 5, 'iterval reads the current value';
}

subtest 'iterating a list', {
    plan 2;
    my $it := nqp::iterator(nqp::list(1, 2, 3));
    my @got;
    while $it { @got.push: nqp::shift($it) }
    is @got.join(','), '1,2,3', 'shift answers each element in order';
    nok nqp::istrue($it), 'the exhausted iterator is false';
}

subtest 'an empty hash', {
    plan 2;
    my $it := nqp::iterator(nqp::hash());
    is nqp::istrue($it), 0, 'an empty iterator is false';
    dies-ok { nqp::shift($it) }, 'shifting past the end dies';
}

subtest 'reading before the first shift', {
    plan 2;
    my $it := nqp::iterator(nqp::hash('x', 5));
    dies-ok { nqp::iterkey_s($it) }, 'iterkey_s before shift dies';
    dies-ok { nqp::iterval($it) }, 'iterval before shift dies';
}

subtest 'the loop JSON-style serializers use', {
    plan 1;
    my %h = one => 1, two => 2;
    my $it := nqp::iterator(nqp::getattr(%h, Map, q/$!storage/));
    my @parts;
    while $it {
        my $e := nqp::shift($it);
        @parts.push: nqp::iterkey_s($e) ~ '=' ~ nqp::iterval($e);
    }
    is @parts.sort.join('&'), 'one=1&two=2', 'keys and values pair up';
}
