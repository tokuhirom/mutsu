use Test;

# `.set_name` stores the name verbatim, and `.name` reads it back verbatim,
# package qualifier included (Sub::Util's `set_subname` gives
# `GLOBAL::foo` / `Foo::foo`).

plan 5;

my $b = { ... };
$b.set_name('GLOBAL::foo');
is $b.name, 'GLOBAL::foo', 'a block keeps a qualified name';

my $s = sub { 1 };
$s.set_name('A::B::c');
is $s.name, 'A::B::c', 'a sub keeps a multi-part qualified name';

$s.set_name('plain');
is $s.name, 'plain', 'an unqualified name stays unqualified';

package P { our sub q { } }
is &P::q.name, 'q', 'a declared package sub is still named without its package';

my &x = sub foo { };
is &x.name, 'foo', 'a named anonymous sub keeps its declared name';
