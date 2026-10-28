use Test;

# `Range.list` / `.List` / `.Array` / `.join` read `self.iterator`, so an
# `is Range` subclass with its own `iterator` lists through it instead of the
# range it carries. Found with Math::Sequences (`class Integers is Range`).
# #15361

plan 6;

class Three does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}
class Nat is Range { method iterator { Three.new } }

is-deeply Nat.new(0, 5).list, (0, 1, 2).list, '.list reads the own iterator';
is-deeply Nat.new(0, 5).List, (0, 1, 2).List, '.List reads the own iterator';
is-deeply Nat.new(0, 5).Array, [0, 1, 2], '.Array reads the own iterator';
is Nat.new(0, 5).join, '012', '.join reads the own iterator';
is Nat.new(0, 5).elems, 6, '.elems is still the range arithmetic';

class Plain is Range { }
is-deeply Plain.new(0, 2).list, (0, 1, 2).list, 'a Range subclass without an iterator lists its range';
