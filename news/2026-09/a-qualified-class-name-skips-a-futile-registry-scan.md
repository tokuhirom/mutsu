# A qualified class name stops scanning the whole class registry

Every `.new` of a user class asks `seed_quanthash_storage` whether the class
inherits a QuantHash base, which resolves the class — and each of its declared
parents — through `resolved_class_parents`. That lookup tries the registry key
verbatim, and on a miss falls back to scanning the class table for a key whose
`::`-tail short name matches, so a bare `Bar` can still find a registered
`Foo::Bar`.

The fallback cannot match a name that is already qualified. A key's `::`-tail is
by construction everything after the *last* `::`, so it never contains one
itself; `short == name` is therefore impossible the moment `name` does. Every
such call scanned the entire table — a `rsplit_once` substring search per entry
— to arrive at the `None` it was always going to return.

That is not a rare shape. A class declared in a module composes its roles by
qualified name, so parsing YAML with the bundled `YAMLish` battery reached here
148 times for names like `YAMLish::Single` and `YAMLish::Quoted`, at about
**97,000 instructions each**: **1.0% of the whole program**, 32,792 substring
searches, to answer a question with no possible answer.

One `name.contains("::")` guard before the fallback removes it:
14,354,235 -> 987,214 Ir for `resolved_class_parents` on a 60-row YAML document.
The short-name fallback itself is untouched, and a bare name still resolves to
its qualified registry entry.

Pinned by `t/collections/set-bag-mix/quanthash-subclass-qualified-name.t`
(verified against rakudo 2026.07): a `BagHash` subclass declared in a module
keeps its mutable backing store, a `Bag` subclass keeps the immutable one, and
a bare parent name still finds the class it was declared as.

Refs [#7576](https://github.com/tokuhirom/mutsu/issues/7576).
