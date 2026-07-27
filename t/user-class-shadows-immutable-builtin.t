use Test;

plan 14;

# A lexical user class shadows an immutable builtin type with the same name.
# Mutation must follow the resolved class declaration and its Hash parent, not
# the spelling of the class name.
class Map is Hash { }
class Set is Hash { }
class Bag is Hash { }
class Mix is Hash { }

my $map = Map.new;
lives-ok { $map<a> = 1 }, 'user Map permits element assignment';
is $map<a>, 1, 'user Map stores the assigned element';
lives-ok { $map<a>:delete }, 'user Map permits element deletion';
nok $map<a>:exists, 'user Map deletes the element';

my $set = Set.new;
lives-ok { $set<a> = 2 }, 'user Set permits element assignment';
is $set<a>, 2, 'user Set stores the assigned element';
lives-ok { $set<a>:delete }, 'user Set permits element deletion';
nok $set<a>:exists, 'user Set deletes the element';

my $bag = Bag.new;
lives-ok { $bag<a> = 3 }, 'user Bag permits element assignment';
is $bag<a>, 3, 'user Bag stores the assigned element';

my $mix = Mix.new;
lives-ok { $mix<a> = 4 }, 'user Mix permits element assignment';
is $mix<a>, 4, 'user Mix stores the assigned element';

# A subclass of the shadowing user Set follows that user declaration's Hash
# parent and is mutable too.
class Child is Set { }
my $child = Child.new;
lives-ok { $child<a> = 5 }, 'a child of the shadowing user Set is mutable';
is $child<a>, 5, 'the child stores the assigned element';
