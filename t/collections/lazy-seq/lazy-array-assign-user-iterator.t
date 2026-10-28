use Test;

# `my @a = <Seq over an is-lazy user iterator>` and an Iterable instance whose
# own `iterator` is lazy assign a LAZY array that reifies on demand, instead of
# draining a never-ending iterator. Found with Math::Sequences (`our @A is
# export = 𝕀` over an `Integers is Range`). #15305

plan 9;

class Count does Iterator {
    has $.n is rw = 0;
    method is-lazy { True }
    method pull-one { $!n++ }
}

my @a = Seq.new(Count.new);
ok @a.is-lazy, 'an array assigned from a Seq over a lazy iterator is lazy';
is-deeply @a[2..4].List, (2, 3, 4), 'elements are produced on demand';
is-deeply @a.head(2).List, (0, 1), '.head over the lazy array';

class Nat does Iterable { method iterator { Count.new } }
my @b = Nat.new;
is-deeply @b[^3].List, (0, 1, 2), 'an Iterable instance with a lazy iterator assigns lazily';

class R is Range { method iterator { Count.new } }
my @r = R.new(0, 5);
is-deeply @r[^3].List, (0, 1, 2), 'a Range subclass with a lazy iterator assigns lazily';

class Fin does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}
my @f = Seq.new(Fin.new);
is-deeply @f.List, (0, 1, 2), 'a finite (non-lazy) iterator is still drained';
ok !@f.is-lazy, '... and gives an eager array';

class FinI does Iterable { method iterator { Fin.new } }
my @g = FinI.new;
is-deeply @g.List, (0, 1, 2), 'a finite Iterable instance is still drained';
ok !@g.is-lazy, '... and gives an eager array';
