use Test;

# ASSIGN-KEY / DELETE-KEY on a Hash and the six quant hashes are rows of the
# method table (ADR-11276 §9.37); one handler answers a named binding, a
# detached container and the backing storage behind a user subclass.

plan 28;

# --- Hash, named binding
my %h = a => 1, b => 2;
my %alias := %h;
is %h.ASSIGN-KEY('c', 3), 3, 'Hash.ASSIGN-KEY answers the value';
is-deeply %alias.sort.List, (:a(1), :b(2), :c(3)), 'an alias sees the store';
is %h.DELETE-KEY('a'), 1, 'Hash.DELETE-KEY answers the old value';
nok %alias<a>:exists, 'an alias sees the removal';
is-deeply %h.DELETE-KEY('zzz'), Any, 'an absent key answers the type object';

# --- typed hash keeps its default and type
my Int %t;
%t.ASSIGN-KEY('x', 5);
is %t<x>, 5, 'typed Hash.ASSIGN-KEY';
is-deeply %t.DELETE-KEY('nope'), Int, 'typed Hash.DELETE-KEY answers its value type';

# --- object hash
my %o{Any};
%o.ASSIGN-KEY(1, 'int');
%o.ASSIGN-KEY('1', 'str');
is %o.elems, 2, 'an object hash keys by WHICH';
is %o.DELETE-KEY(1), 'int', 'object Hash.DELETE-KEY';
is %o.elems, 1, 'one key left';

# --- by-value receiver
sub mk { %h }
is mk().ASSIGN-KEY('v', 9), 9, 'a by-value receiver';
is %h<v>, 9, 'the shared node saw the store';

# --- a bound entry refuses assignment
my %b;
%b.BIND-KEY('k', 42);
throws-like { %b.ASSIGN-KEY('k', 1) }, X::AdHoc, message => /immutable/, 'an entry bound to a bare value is read-only';

# --- SetHash
my $s = SetHash.new(<a b>);
is $s.ASSIGN-KEY('c', True), True, 'SetHash.ASSIGN-KEY answers the value';
ok $s<c>, 'the element is in';
is $s.ASSIGN-KEY('c', False), False, 'a false value removes it';
nok $s<c>, 'the element is out';
is $s.DELETE-KEY('a'), True, 'SetHash.DELETE-KEY answers whether it was in';
is $s.DELETE-KEY('a'), False, 'and False the second time';

# --- BagHash / MixHash
my $bag = BagHash.new(<a a b>);
is $bag.ASSIGN-KEY('c', 4), 4, 'BagHash.ASSIGN-KEY';
is $bag<c>, 4, 'the count is stored';
is $bag.DELETE-KEY('a'), 2, 'BagHash.DELETE-KEY answers the old count';
nok $bag<a>:exists, 'the key is gone';
my $mix = MixHash.new-from-pairs(a => 1.5, b => 2);
is $mix.ASSIGN-KEY('c', 0.5), 0.5, 'MixHash.ASSIGN-KEY';
$mix.DELETE-KEY('a');
nok $mix<a>:exists, 'MixHash.DELETE-KEY removes the key';

# --- immutable quant hashes
throws-like { Set.new(<a>).ASSIGN-KEY('b', True) }, X::Assignment::RO, 'Set is immutable';
throws-like { Bag.new(<a>).DELETE-KEY('a') }, X::Assignment::RO, 'Bag is immutable';

# --- the backing storage behind a user subclass
class Logged is Hash {
    has @.log;
    method ASSIGN-KEY(\k, \v) { @!log.push("set {k}"); nextsame }
    method DELETE-KEY(\k) { @!log.push("del {k}"); nextsame }
}
my %l is Logged;
%l.ASSIGN-KEY('q', 7);
is %l<q>, 7, 'nextsame reaches the Hash row on the backing storage';

# vim: expandtab shiftwidth=4
