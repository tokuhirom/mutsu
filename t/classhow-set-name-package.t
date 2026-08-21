use v6;
use Test;

plan 6;

# `.^set_name` on a user-declared class's own `Package` type object (as
# opposed to a `Mixin`-wrapped value, which `t/metamodel-set-name.t` covers)
# must rename it, and the rename must be visible on later `.^name` reads.
# See todo/tickets/set-name-on-builtin-type-package-no-op.md.

class Foo {}
is Foo.^name, 'Foo', 'user class reports its declared name before set_name';

my $ret = Foo.^set_name('Foo(restricted)');
is $ret, 'Foo(restricted)', 'set_name returns the new name';
is Foo.^name, 'Foo(restricted)', '.^name reflects the set name on a Package value';
is Foo.HOW.name(Foo), 'Foo(restricted)', 'HOW.name reflects the set name too';

# Safety: renaming a builtin type's shared `Package` value (reached e.g. via
# `.WHAT` on a native value) must NOT globally rename that type for every
# other value of it — Rakudo's own metamodel gives a role-mixed value's
# `.WHAT` a distinct anonymous type object rather than reusing the shared
# builtin, so `.^set_name` on the shared builtin `Package` value itself is a
# safe no-op in mutsu rather than an actively-wrong process-wide rename.
my %h;
Hash.^set_name('Hash(restricted)');
is Hash.^name, 'Hash', 'set_name on the shared builtin Hash package is a no-op';
is %h.^name, 'Hash', 'an unrelated hash is unaffected by the attempted builtin rename';
