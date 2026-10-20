use Test;

# `Any.elems` answers every receiver the zero-argument cascade used to (a
# junction's eigenstates, a buffer, a lazy list, a stash, a user object), and
# `Array.default` / `Hash.default` answer a typed, shaped or lazy container
# from their own rows (ADR-11276 §9.163).

plan 27;

class P { }
class Q { method list { (1, 2, 3) } }

is 5.elems, 1, 'a scalar is one element';
is Int.elems, 1, 'a type object is one element';
is (a => 1).elems, 1, 'a Pair is one element';
is (1 .. 10).elems, 10, 'a Range counts its elements';
is bag(1, 1, 2).elems, 2, 'a Bag counts its keys';
is Buf.new(1, 2, 3).elems, 3, 'a Buf counts its bytes';
is (gather { take 1; take 2 }).elems, 2, 'a gather counts its elements';
is (^3).map({ $_ }).elems, 3, 'a map over a finite source counts';
is P.new.elems, 1, 'a plain object is one element';
is Q.new.elems, 3, 'an object with its own list counts it';
is \(1, 2, :a).elems, 2, 'a Capture counts its positionals';
is Date.new(2026, 1, 1).elems, 1, 'a Date is one element';
my @shaped[2;3];
is @shaped.elems, 2, 'a shaped array counts its first dimension';
isa-ok (1 .. *).elems, Failure, 'an infinite Range has no count';
my @lazy = lazy 1 .. *;
isa-ok @lazy.elems, Failure, 'a lazy array has no count';
isa-ok (lazy gather { take 1 }).elems, Failure, 'a lazy gather has no count';
package Stashed { our $x = 1; our $y = 2 }
is Stashed::.elems, 2, 'a stash counts its symbols';

my @a is default(5);
is @a.default, 5, 'Array.default with `is default`';
my @b;
is @b.default.raku, 'Any', 'an untyped array defaults to Any';
my Int @c;
is @c.default.raku, 'Int', 'a typed array defaults to its element type';
my %h is default(3);
is %h.default, 3, 'Hash.default with `is default`';
my Int %ti;
is %ti.default.raku, 'Int', 'a typed hash defaults to its value type';
is [1, 2].default.raku, 'Any', 'an Array literal defaults to Any';
throws-like { (1, 2).default }, X::Method::NotFound, 'a List has no default';

# A lazy array answers its container's default without being reified (#13806).
my @lazy-plain = lazy 1..*;
is @lazy-plain.default.raku, 'Any', 'a lazy array defaults to Any';
my Int @lazy-typed = lazy 1..*;
is @lazy-typed.default.raku, 'Int', 'a typed lazy array defaults to its type';
my @lazy-default is default(5) = lazy 1..*;
is @lazy-default.default, 5, 'a lazy array with `is default`';
