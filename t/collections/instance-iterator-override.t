use Test;

plan 12;

# A plain class (no `does Iterable`, no `is Array`/`is List`) that defines its
# own `iterator` method: Rakudo routes Any's ITERATION methods through that
# override regardless of role composition -- measured against raku (#8547).
class Wrapper {
    has @.items;
    method iterator { @.items.iterator }
}

my $w = Wrapper.new(items => [1, 2, 3, 4, 5]);

ok $w !~~ Iterable, 'the class does not compose Iterable';

is $w.map({ $_ * 2 }).elems, 5, 'the ticket repro: map decomposes via the override';
is-deeply $w.map({ $_ * 2 }).List, (2, 4, 6, 8, 10), 'map applies the block to each element';
is-deeply $w.grep({ $_ > 2 }).List, (3, 4, 5), 'grep decomposes via the override';
is-deeply $w.sort.List, (1, 2, 3, 4, 5), 'sort decomposes via the override';
is $w.first, 1, 'first decomposes via the override';
is $w.head, 1, 'head decomposes via the override';
is $w.tail, 5, 'tail decomposes via the override';

# Methods raku does NOT route through `.iterator` for a plain (non-Iterable)
# class: they stay a single opaque item. `flat` is the interesting case here
# -- it DOES follow the override for an `is Array`/`does Iterable` class
# (t/collections/array/array-subclass-iterator-override.t), but not for a
# plain class with no role composition at all -- measured against raku.
is $w.elems, 1, 'elems treats the instance as a single item';
is-deeply $w.list.List, ($w,), 'list treats the instance as a single item';
is-deeply $w.flat.List, ($w,), 'flat treats the instance as a single item';

# A closer reduction of the Game::Entities repro from #8496/#8547: a `.map`
# result feeds straight into another list-consuming method.
class View {
    has @.items;
    method iterator { @.items.iterator }
}
my $view = View.new(items => [1, 1, 1, 1, 1]);
is $view.map({ 1 }).sum, 5, 'a map result over the override sums correctly';
