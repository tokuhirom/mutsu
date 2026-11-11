use Test;

# A `$` scalar whose name starts with an uppercase letter (`our $Baz`, a stash
# bind of `'$Baz'`) is the stash entry `$Baz`, not a term `Baz`. The store
# spells a scalar's key without its sigil, so the uppercase spelling used to
# read as a type. From CompUnit::Util's `set-in-WHO(..., 'Foo::Bar::$Baz', ...)`.
plan 9;

package P {
    our $Baz = 1;
    our $low = 2;
    our sub Foo() { }
    class Cls { }
    our constant K = 3;
}
is-deeply P::.keys.sort.List, ('$Baz', '$low', '&Foo', 'Cls', 'K'),
    'an uppercase our scalar lists with its sigil beside types and terms';
is P::<$Baz>, 1, 'the uppercase our scalar reads through the stash';
is $P::Baz, 1, 'and through its qualified name';

my package tmp { };
BEGIN {
    my $w = tmp.WHO;
    $w.package_at_key('Foo');
    my $w2 = $w<Foo>.WHO;
    $w2<$Qux> := 'q';
    $w2<$qux> := 'l';
}
is-deeply tmp::Foo.WHO.keys.sort.List, ('$Qux', '$qux'),
    'a bound uppercase scalar lists with its sigil';
is tmp::Foo::<$Qux>, 'q', 'the bound uppercase scalar reads back';
is tmp::Foo::<$qux>, 'l', 'a lowercase one still does';

my package nest { };
BEGIN {
    nest.WHO.package_at_key('A');
    nest.WHO<A>.WHO.package_at_key('B');
    nest.WHO<A>.WHO<B>.WHO<$Baz> := 'bar';
}
is nest::A::B::<$Baz>, 'bar', 'a scalar bound two packages down';
is-deeply nest::A::B.WHO.keys.List, ('$Baz',), 'lists as $Baz';

my package rebind { };
BEGIN {
    rebind.WHO<Thing> := 'term';
    rebind.WHO<$Other> := 'scalar';
}
is-deeply rebind.WHO.keys.sort.List, ('$Other', 'Thing'),
    'a term and a scalar bound side by side keep their spellings';
