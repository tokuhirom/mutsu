use v6;
use lib 't/lib';
use Test;
use CompoundNameScope;

plan 7;

# The `::` segments of a COMPOUND DECLARED NAME are not lexical scopes.
# `class Foo::List { ... }` at file scope puts only a `Foo` package stub in the
# file's scope in raku; `Foo`'s contents are NOT visible unqualified inside the
# body. mutsu derived the enclosing-package chain by splitting the registered
# name, so `Foo` looked like a real scope and the class found ITSELF under its
# own short name -- `List.new(1, 2, 3)` inside `class Foo::List` constructed a
# `Foo::List` instead of a core List. That is the `Crane::List` / `Crane::Set`
# shape every `unit class Crane::X;` file is written in.
#
# Only TYPE names are pinned here. Bare *routine* lookup still crosses those
# segments on purpose: mutsu also uses them to model a module compunit's
# file-scope lexicals (`HTTP::HPACK`'s own `sub decode-int`, reached from
# `HTTP::HPACK::Decoder`'s methods), so cutting the walk there breaks bundled
# modules. See `bare_name_packages`.

class Foo::List {
    method core-list-name() { List.^name }
    method make-a-list() { List.new(1, 2, 3) }
}

is Foo::List.core-list-name, 'List',
    'a bare core type name inside `class Foo::List` is the CORE List';
is-deeply Foo::List.make-a-list, (1, 2, 3),
    'and `List.new` inside it builds a core List';

class Baz::Hash {
    method core-hash-name() { Hash.^name }
}
is Baz::Hash.core-hash-name, 'Hash',
    'same for a compound-named class whose tail is `Hash`';

grammar Qux::Match {
    method core-match-name() { Match.^name }
}
is Qux::Match.core-match-name, 'Match',
    'same for a compound-named grammar';

class Corge::Set {
    method core-set-name() { Set.^name }
    method make-a-set() { Set.new(1, 2) }
}
is Corge::Set.core-set-name, 'Set',
    'same when the tail shadows a core role-backed type';

# Real lexical nesting must keep working: a class declared INSIDE a module is
# registered module-qualified too, and there the outward walk is correct.
is CompoundNameScope::Searcher.call-helper, 'module helper',
    'a class lexically inside a module still sees the module\'s routines';

module Deep {
    our sub deep-helper() { 'deep helper' }
    class Inner {
        method call() { deep-helper() }
    }
}
is Deep::Inner.call, 'deep helper',
    'and so does a class inside an in-file module block';
