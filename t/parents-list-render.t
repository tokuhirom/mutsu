use Test;

# `.^parents` returns a List in raku (its `.WHAT` is `(List)` and it gists with
# parens `(...)`), like `.^roles` and `.^mro`. mutsu returned a real Array, so
# it gisted as `[...]` and `.WHAT` was `(Array)`.

plan 8;

class Animal { }
class Dog is Animal { }

is Dog.^parents.WHAT.gist, '(List)', '.^parents is a List';
is Dog.^parents.raku, '(Animal,)', '.^parents.raku uses List parens';
is Dog.^parents.gist, '((Animal))', '.^parents gists with List parens';
is Dog.new.^parents.WHAT.gist, '(List)', '.^parents on an instance is a List';

# :all keeps the List type
is Dog.^parents(:all).WHAT.gist, '(List)', '.^parents(:all) is a List';
is Dog.^parents(:all).elems, 3, ':all lists Animal, Any, Mu';

# Elements are still the parent type objects (assignment to @ works)
my @p = Dog.^parents;
ok @p[0] =:= Animal, 'first parent element is Animal';
is @p.elems, 1, 'one immediate parent';
