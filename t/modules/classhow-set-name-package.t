use v6;
use Test;

plan 9;

# `.^set_name` on a `Package` type object -- whether a user-declared class's
# own type object, or a builtin's shared type object (`Hash`, `Int`, ...; as
# opposed to a `Mixin`-wrapped value, which `t/metamodel-set-name.t` covers)
# -- must rename it, and the rename must be visible on later `.^name` reads,
# via every read path: plain `.^name`, `.HOW.name(...)`, and on a concrete
# VALUE of the renamed type, not just the type object itself.
# See news/2026-08/set-name-builtin-type-process-wide-rename.md.

class Foo {}
is Foo.^name, 'Foo', 'user class reports its declared name before set_name';

my $ret = Foo.^set_name('Foo(restricted)');
is $ret, 'Foo(restricted)', 'set_name returns the new name';
is Foo.^name, 'Foo(restricted)', '.^name reflects the set name on a Package value';
is Foo.HOW.name(Foo), 'Foo(restricted)', 'HOW.name reflects the set name too';

# A builtin type's shared `Package` value (reached e.g. via `.WHAT` on a
# native value) really is the SAME object every value of that type points
# to, so renaming it renames the type process-wide -- matching real Rakudo:
# `Hash.^set_name("X")` there makes every `%h.^name` report "X" too, not
# just `Hash.^name` itself (verified against `raku`). A role-mixed value's
# `.WHAT` (see `docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md`
# and `t/metamodel-set-name.t`) is what gives `Hash::Restricted` a distinct
# per-composition anonymous type object to rename instead, when a caller
# wants a scoped rename rather than a global one.
my %h;
Hash.^set_name('Hash(restricted)');
is Hash.^name, 'Hash(restricted)', 'set_name on the shared builtin Hash package renames it process-wide';
is %h.^name, 'Hash(restricted)', 'an unrelated pre-existing hash observes the same rename';
is %h.HOW.name(%h), 'Hash(restricted)', 'HOW.name on a plain hash value observes the same rename too';

# The read side for a concrete builtin VALUE (not its `Package`/`.WHAT`)
# goes through a different fallback than Hash's container-type-metadata
# path above -- exercise it with a plain Int to confirm it generalizes.
my $renamed-int-name = Int.^set_name('Int(renamed)');
is $renamed-int-name, 'Int(renamed)', 'set_name on Int returns the new name';
is 5.^name, 'Int(renamed)', 'a plain Int VALUE observes the Int rename too';
