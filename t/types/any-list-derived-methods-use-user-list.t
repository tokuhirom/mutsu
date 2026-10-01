use Test;

plan 12;

# `Any`'s list-derived methods are `self.list.<method>`, so a user class that
# defines `method list` gets them for free (Math::Matrix relies on this).
class M { has @.v = 1, 2, 3, 4; method list { @!v.list } }
my $m = M.new;

is $m.elems, 4, '.elems goes through the user list';
is $m.Slip.raku, 'slip(1, 2, 3, 4)', '.Slip goes through the user list';
is-deeply [|$m], [1, 2, 3, 4], 'prefix | slips the user list';
is (|$m).elems, 4, '(|$m) holds the list elements';
is (1, |$m).elems, 5, '|$m inside a list constructor';
sub count(|c) { c.elems }
is count(|$m), 4, '|$m in a call';
is $m.hash.elems, 2, '.hash goes through the user list';
is (%$m).elems, 2, '%$m goes through the user list';
is $m.flat.elems, 4, '.flat goes through the user list';
is $m.Array.elems, 4, '.Array goes through the user list';

# A class without its own `list` has no pairs to build a hash from.
class N { has @.v = 1, 2, 3, 4 }
throws-like { N.new.hash }, X::Hash::Store::OddNumber, 'Any.hash on a plain class';

# A class's own method still wins.
class O { method list { 1, 2 }; method elems { 42 } }
is O.new.elems, 42, 'an own .elems overrides the Any default';
