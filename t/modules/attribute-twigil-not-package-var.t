use v6;
use Test;

# An attribute container (`@!items`) shares its bare spelling with a package
# variable (`our @items`) of the class's own package. A method's
# `@!items.push`/`.List`/`:=` and an `our @items` read inside the same package
# must each reach their own container: the attribute is never looked up as a
# package variable, the package variable still is.

plan 6;

package Store {
    our @items = <p q>;

    class Box {
        has @.items;
        submethod TWEAK { @!items := @!items.List }
        method count { @!items.elems }
        method pkg-items { @items.join(',') }
    }

    class Bag {
        has @.items;
        method add($x) { @!items.push($x); self }
    }

    our sub pkg-count { @items.elems }
}

my $box = Store::Box.new(items => (1, 2, 3));
is $box.count, 3, 'TWEAK rebinding an attribute to its own .List';
ok $box.items ~~ List, 'the attribute holds the List';
is $box.pkg-items, 'p,q', 'the same-named package variable from a method';

my $bag = Store::Bag.new;
$bag.add(1).add(2);
is $bag.items.join(','), '1,2', 'pushing to an attribute container';
is Store::pkg-count(), 2, 'the package variable is untouched by the attributes';
is @Store::items.join(','), 'p,q', 'and reads the same qualified';
