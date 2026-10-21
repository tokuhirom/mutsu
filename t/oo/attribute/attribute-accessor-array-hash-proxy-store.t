use Test;
use nqp;

plan 3;

# `$obj.attr = v` on an `is rw` `@`/`%` attribute that holds a Proxy is the
# Proxy's STORE, as it is for a `$` attribute (#14266, AttrX::Mooish).
my @stored;
class C {
    has $.s is rw;
    has @.a is rw;
    has %.h is rw;
    method bind {
        nqp::bindattr(self, C, '$!s', Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $v { @stored.push: "s " ~ $v.raku }));
        nqp::bindattr(self, C, '@!a', Proxy.new(FETCH => -> $ { [1] }, STORE => -> $, $v { @stored.push: "a " ~ $v.raku }));
        nqp::bindattr(self, C, '%!h', Proxy.new(FETCH => -> $ { {a => 1} }, STORE => -> $, $v { @stored.push: "h " ~ $v.raku }));
    }
}

my $c = C.new;
$c.bind;
$c.s = 5;
is-deeply @stored, ['s 5'], 'scalar attribute: STORE';
@stored = ();
$c.a = [2, 3];
is-deeply @stored, ['a $[2, 3]'], 'array attribute: STORE';
@stored = ();
$c.h = {x => 2};
is-deeply @stored, ['h ${:x(2)}'], 'hash attribute: STORE';
