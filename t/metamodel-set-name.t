use v6;
use Test;

plan 6;

# `.^set_name` renames a metaobject; most often applied to a freshly-composed
# anonymous type (e.g. `Foo.new but role {...}`) to give it a readable name.

class Foo { has $.x = 1; }

my $obj = Foo.new but role :: { has $.tag = 'hello' };
is $obj.^name, 'Foo', 'mixed-in object reports its base name before set_name';

my $ret = $obj.^set_name('MyFriendlyName');
is $ret, 'MyFriendlyName', 'set_name returns the new name';
is $obj.^name, 'MyFriendlyName', '.^name reflects the set name';
is $obj.HOW.name($obj), 'MyFriendlyName', 'HOW.name reflects the set name';

# The mixed-in attribute still works after renaming.
is $obj.tag, 'hello', 'mixed-in role attribute survives set_name';

# An alias sharing the same object observes the rename (in-place metaobject).
my $alias = $obj;
is $alias.^name, 'MyFriendlyName', 'aliased object observes the rename';
