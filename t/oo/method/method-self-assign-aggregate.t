use Test;

plan 5;

# `self = ...` in a method of a role mixed into a Hash/Array value assigns the
# aggregate in place (Hash::LRU's `method clear() { self = () }`); it used to
# die with "Cannot modify an immutable value" because the invocant arrived as
# the value itself rather than through a container cell.
role HR {
    method fill()  { self = (a => 1, b => 2) }
    method clear() { self = () }
}
my %h = x => 0;
%h does HR;
%h.fill;
is-deeply %h.sort.List, (a => 1, b => 2), 'self = pairs replaces the mixed-in hash';
%h.clear;
is %h.elems, 0, 'self = () empties it';
ok %h ~~ HR, 'the role survives the reassignment';

role AR { method clear() { self = () } }
my @a = 1, 2, 3;
@a does AR;
@a.clear;
is @a.elems, 0, 'self = () empties a mixed-in array';

# A plain object invocant is still immutable.
class P { method boom { self = 1 } }
dies-ok { P.new.boom }, 'assigning to an object self still dies';
