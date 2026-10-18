use Test;

# Rows of the built-in roles a user class composes, and the sampling methods'
# `Callable` count (ADR-11276 §9.165). Expected values are Rakudo's.

plan 19;

# Positional.of, Associative.of / keyof.
{
    class A does Positional[Int] { }
    is A.of.^name, 'Int', 'Positional.of on the type object';
    is A.new.of.^name, 'Int', '... and on an instance';
    role R[::T] does Positional[T] { }
    class B does R[Str] { }
    is B.of.^name, 'Str', 'through a parametric role';
    class C is Hash does Associative[Cool, DateTime] { }
    is C.of.^name, 'Cool', 'Associative.of';
    is C.keyof.^name, 'DateTime', 'Associative.keyof';
    class D is C { }
    is D.new.of.^name, 'Cool', 'through a subclass';
    is Array[Int].of.^name, 'Int', 'a parameterized type object keeps its own';
}

# Iterable's hyper / race / lazy of a user Iterable.
{
    class I does Iterable { method iterator { (1, 2, 3).iterator } }
    is-deeply I.new.hyper.map(* + 1).list, (2, 3, 4), 'Iterable.hyper';
    is-deeply I.new.race.map(* + 1).sort.list, (2, 3, 4), 'Iterable.race';
    ok I.new.lazy.is-lazy, 'Iterable.lazy';
}

# A Callable count is called with the receiver's size.
{
    my $b = bag <a a b c>;
    is $b.pick(* - 1).elems, 3, 'Bag.pick: total - 1';
    is $b.roll(* div 2).elems, 2, 'Bag.roll';
    is $b.pickpairs(* - 1).elems, 2, 'Bag.pickpairs: elems - 1';
    my $bh = <a a b c>.BagHash;
    is $bh.grab(* - 2).elems, 2, 'BagHash.grab';
    is $bh.total, 2, '... removes what it grabbed';
    is (1..10).pick(* div 2).elems, 5, 'Range.pick';
    my @a = 1..6;
    is @a.grab(* div 3).elems, 2, 'Array.grab';
    is @a.elems, 4, '... removes what it grabbed';
    is Bool.pick(* - 1).elems, 1, 'Bool.pick counts its two values';
}
