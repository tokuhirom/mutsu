use Test;

# Writing a routine into a named package's stash -- `Foo::{'&bar'} = ...`,
# `Foo::<&bar> := ...` -- installs it as that package's symbol, and when the
# package is a module's `EXPORT::DEFAULT` it becomes one of the module's
# exports. mutsu wrote such stores into a throwaway copy of the stash, so the
# routine was neither callable nor exported ("Unknown function: tags" loading
# Sparky-Job-Api, whose dependency `Sparrow6::DSL` re-exports this way).

plan 8;

use lib 't/lib';
use NamedStashReExport;

is tags()<project>, 'demo', 'a sub re-exported through a runtime stash key';
is shout('hi'), 'HI', 'every key the BEGIN loop wrote is exported';
is own(), 'own', 'a literal `&` key bound with := is exported too';

package Foo { }
Foo::<&bar> = sub { 'bar' };
is Foo::bar(), 'bar', 'a literal `&` key installs a package routine';

my $key = '&baz';
Foo::{$key} := sub ($n) { "baz $n" };
is Foo::baz(1), 'baz 1', 'a runtime `&` key installs a package routine';
ok Foo::<&baz>.defined, 'and reads back through the stash';

Foo::<$x> = 42;
is Foo::<$x>, 42, 'a scalar key still assigns through the stash';

Foo::.BIND-KEY('&qux', sub { 'qux' });
our &Foo::quux = sub { 'quux' };
is Foo::.keys.sort, ('$x', '&bar', '&baz', '&quux', '&qux'),
    'every sigil-leading routine member is listed in the stash';
