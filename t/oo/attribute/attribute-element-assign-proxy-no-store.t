use Test;
use nqp;

plan 6;

# `$obj.attr[i] = v` / `$obj.attr<k> = v` on a Proxy-bound `@`/`%` attribute
# stores into the aggregate FETCH answered, in place, and does not call STORE:
# the attribute's trigger fires on assigning the attribute, not an element of
# it (#14266, AttrX::Mooish's `t/011-arrays`).
my @backing = 1, 2, 3;
my %backing = a => 1;
my @stored;
class C {
    has @.a is rw;
    has %.h is rw;
    method bind {
        nqp::bindattr(self, C, '@!a', Proxy.new(FETCH => -> $ { @backing }, STORE => -> $, $v { @stored.push: "a" }));
        nqp::bindattr(self, C, '%!h', Proxy.new(FETCH => -> $ { %backing }, STORE => -> $, $v { @stored.push: "h" }));
    }
}
my $c = C.new;
$c.bind;

$c.a[1] = 9;
is-deeply @backing, [1, 9, 3], 'array element stored into the fetched array';
$c.h<b> = 2;
is-deeply %backing, {a => 1, b => 2}, 'hash element stored into the fetched hash';
is-deeply @stored, [], 'neither element store calls STORE';

$c.a = [7, 8];
is-deeply @stored, ['a'], 'assigning the array attribute calls STORE';
$c.h = {z => 1};
is-deeply @stored, ['a', 'h'], 'assigning the hash attribute calls STORE';
$c.a[5] = 1;
is @backing.elems, 6, 'an element past the end grows the fetched array';
