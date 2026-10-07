use Test;

# `Array`'s `push`, `append`, `unshift`, `prepend`, `pop`, `shift` and `splice`,
# and the `List` rows that refuse them, are rows of the one method table
# (ADR-11276 §9.23): `Handler::Mut` rows that write through the array's shared
# node and read the variable's declared types by name when the receiver has one.
# One row answers every receiver the six former copies served: an `@` variable,
# a scalar that holds an array, a by-value array, an `is Array` instance and the
# array a mixin wraps.

plan 81;

# --- the six simple mutators on an `@` variable ---------------------------------
my @a = 1, 2;
is-deeply @a.push(3), [1, 2, 3], 'push answers the array';
@a.push(4, 5);
is-deeply @a, [1, 2, 3, 4, 5], 'push takes several elements';
@a.push((6, 7));
is @a.elems, 6, 'a single list argument is one element for push';
is-deeply @a.pop, $(6, 7), 'pop answers the last element';

@a.append(8, 9);
is-deeply @a, [1, 2, 3, 4, 5, 8, 9], 'append takes several';
@a.append((10, 11));
is-deeply @a, [1, 2, 3, 4, 5, 8, 9, 10, 11], 'a single list argument is flattened by append';
is-deeply @a.shift, 1, 'shift answers the first element';

@a.unshift(0, -1);
is-deeply @a[0, 1, 2], (0, -1, 2), 'unshift puts its elements first, in order';
@a.prepend((-3, -2));
is-deeply @a[0, 1], (-3, -2), 'prepend flattens a single list argument';

# an empty array
my @e;
is-deeply @e.push, [], 'push of nothing is the array';
isa-ok @e.pop, Failure, 'pop of an empty array is a Failure';
isa-ok @e.shift, Failure, 'shift of an empty array is a Failure';
is @e.pop.exception.message, 'Cannot pop from an empty Array', 'whose message names the container';

# a call longer than any mask goes (a spread argument)
my @long;
@long.push(|(1..20));
is @long.elems, 20, 'push takes a spread list of twenty';
@long.unshift(1, 2, 3, 4, 5, 6, 7, 8, 9);
is @long.elems, 29, 'unshift takes nine arguments';
@long.append(|(1..10), 11);
is @long.elems, 40, 'append takes eleven';

# --- argument errors ---------------------------------------------------------------
throws-like { @a.pop(1) }, X::AdHoc, message => /'Too many positionals'/, 'pop takes no argument';
throws-like { @a.shift(1) }, X::AdHoc, message => /'Too many positionals'/, 'shift takes no argument';
is-deeply @e.push(:zzz), [], 'an undeclared named argument is not an element';
is-deeply (@e.pop(:zzz)).defined, False, 'nor an argument of pop';

# --- Nil decays to the element default ------------------------------------------
my @n = 1;
@n.push(Nil);
is-deeply @n, [1, Any], 'a Nil element is the default';
my @d is default(42) = 1, 2, 3;
@d.push(Nil);
is-deeply @d, [1, 2, 3, 42], 'an `is default` container supplies it';
@d.append(Nil);
is-deeply @d[4], 42, 'for append too';
@d.unshift(Nil);
is-deeply @d[0], 42, 'for unshift';
@d.prepend(Nil);
is-deeply @d[0], 42, 'for prepend';

# --- typed and native arrays ---------------------------------------------------------
my Int @t;
@t.push(1);
throws-like { @t.push('x') }, X::TypeCheck::Assignment, 'push checks the element type';
throws-like { @t.append(1, 'x') }, X::TypeCheck::Assignment, 'append checks every element';
throws-like { @t.unshift('x') }, X::TypeCheck::Assignment, 'unshift checks it';
throws-like { @t.prepend('x') }, X::TypeCheck::Assignment, 'prepend checks it';
@t.push(Nil);
is-deeply @t[1].WHAT, Int, 'a Nil in a typed array is its type object';
is @t.pop.WHAT.gist, '(Int)', 'and so is what pop answers';

my uint8 @e8;
@e8.push(1, 300, 2);
is @e8.join(','), '1,44,2', 'a native integer array wraps what it stores';

# --- the declared type is kept across the write --------------------------------------
my Int @kept = 1, 2;
@kept.push(3);
@kept.shift;
is @kept.of.gist, '(Int)', 'push and shift keep the element type of the container';

# --- an immutable List ------------------------------------------------------------------
my $l = (1, 2, 3);
for <push pop shift unshift append prepend> -> $m {
    throws-like { $m eq 'pop' | 'shift' ?? $l."$m"() !! $l."$m"(4) }, X::Immutable,
        method => $m, "List.$m is immutable";
}
throws-like { $l.splice(0, 1) }, X::Multi::NoMatch, 'a List has no splice candidate';
throws-like { (1, 2).push(3) }, X::Immutable, 'a literal List';
my @bound := (1, 2);
throws-like { @bound.push(3) }, X::Immutable, 'an `@` bound to a List';

# --- splice ----------------------------------------------------------------------------
my @s = 1..6;
is-deeply @s.splice(1, 2), [2, 3], 'splice answers the removed elements';
is-deeply @s, [1, 4, 5, 6], 'and removes them';
@s.splice(1, 0, 'a', 'b');
is-deeply @s, [1, 'a', 'b', 4, 5, 6], 'splice inserts the replacement';
is-deeply @s.splice(*-2), [5, 6], 'a WhateverCode offset counts from the end';
is-deeply @s.splice(1, *-1), ['a', 'b'], 'a WhateverCode size too';
my @whole = 1..3;
is-deeply @whole.splice, [1, 2, 3], 'splice with no arguments removes everything';
is @whole.elems, 0, 'leaving it empty';
my @self = 1, 2;
@self.splice(1, 0, @self);
is-deeply @self, [1, 1, 2, 2], 'a self-splice replaces with a snapshot';
throws-like { @s.splice(10, 0, 1) }, X::OutOfRange, 'an offset past the end is out of range';
throws-like { @s.splice(0, -1) }, X::OutOfRange, 'a negative size is out of range';
throws-like { @s.splice('x', 1) }, X::Multi::NoMatch, 'a Str offset matches no candidate';
throws-like { @s.splice(0, 0, (1..*).lazy) }, X::Cannot::Lazy, 'a lazy replacement is refused';
my Int @ts = 1, 2, 3;
throws-like { @ts.splice(1, 0, 'x') }, X::TypeCheck::Splice, 'splice checks the replacement type';

# --- a scalar holding an array, a bound alias ----------------------------------------------
my $r = [1, 2];
$r.push(3);
$r.unshift(0);
is-deeply $r, [0, 1, 2, 3], 'push and unshift on a scalar holding an array';
is-deeply $r.pop, 3, 'pop on it';
$r.splice(1, 1);
is-deeply $r, [0, 2], 'splice on it';
my @src = 1, 2;
my $alias := @src;
$alias.push(3);
is-deeply @src, [1, 2, 3], 'a scalar bound to an array writes through';
sub addto(@x) { @x.push(9) }
my @w;
addto(@w);
is-deeply @w, [9], 'an array passed to a routine';
my $nil;
$nil.push(1);
is-deeply $nil, [1], 'push on an undefined scalar vivifies an array';

# --- by value --------------------------------------------------------------------------------
sub mk { state @s = 1, 2, 3; @s }
mk().push(4);
is-deeply mk(), [1, 2, 3, 4], 'a by-value receiver is the same container';
is-deeply mk().pop, 4, 'pop on it';
is-deeply [1, 2, 3].shift, 1, 'a literal array';
is-deeply [1, 2, 3].splice(1, 1), [2], 'splice on a literal array';

# --- attributes and nested elements ------------------------------------------------------------
class Q {
    has @.items;
    method add($x) { @!items.push($x); self }
    method take { @!items.shift }
}
my $q = Q.new;
$q.add(1).add(2);
is-deeply $q.items, [1, 2], 'an attribute';
is $q.take, 1, 'shift on an attribute';
my @rows = [1, 2], [3];
@rows[0].push(9);
is-deeply @rows[0], [1, 2, 9], 'an element that is an array';
@rows.push([4]);
is @rows.elems, 3, 'pushing an array onto an array of arrays';

# --- an `is Array` instance and its subclasses ----------------------------------------------
class V is Array { }
my $v = V.new(1, 2);
$v.push(3);
$v.unshift(0);
is $v.join(','), '0,1,2,3', 'push and unshift on an `is Array` instance';
is $v.pop, 3, 'pop on it';
is $v.shift, 0, 'shift on it';
$v.append(7, 8);
is $v.elems, 4, 'append on it';

# --- the method augmented onto Array reaches the rows -------------------------------------------
use MONKEY-TYPING;
augment class Array {
    method push-twice($x) { self.push($x); self.push($x) }
}
my @aug;
@aug.push-twice(5);
is-deeply @aug, [5, 5], 'a method augmented onto Array calls the rows';

# --- a container shared by a closure ---------------------------------------------------------
my @shared;
my &adder = { @shared.push($^v) };
adder(1);
adder(2);
is-deeply @shared, [1, 2], 'an array captured by a closure';

# --- the Array.grab row ------------------------------------------------------------------------
my @g = 1..5;
my $got = @g.grab;
is @g.elems, 4, 'grab removes one element';
ok $got ~~ 1..5, 'and answers it';

{
    # pop/shift on an empty typed array name the container as Array[T] (#12242)
    my Int @t;
    is @t.pop.exception.message, 'Cannot pop from an empty Array[Int]', 'pop on empty Array[Int]';
    is @t.shift.exception.message, 'Cannot shift from an empty Array[Int]', 'shift on empty Array[Int]';
    my @u;
    is @u.pop.exception.message, 'Cannot pop from an empty Array', 'pop on empty untyped Array';
    my Str @s;
    is @s.shift.exception.message, 'Cannot shift from an empty Array[Str]', 'shift on empty Array[Str]';
}

done-testing;
