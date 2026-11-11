# A nested package's term no longer replaces its parent's stash entry

A package stash is assembled from the flat `our` store, where a nested package
created through `Stash.package_at_key` lives under its full name
(`tmp::Foo`, `tmp::Foo::Bar`). Seen from `tmp`, the key `tmp::Foo::Bar`
contributes the sub-package `Foo` — but the `our`-store loop published it
under `Foo` *with `tmp::Foo::Bar`'s value*, and the first candidate won. So
`tmp.WHO<Foo>` could answer `tmp::Foo::Bar`, and a keyed `tmp::<Foo>` read the
same wrong value; which one won depended on the order the names were interned,
so mentioning `tmp::Foo::Bar::` anywhere in the program was enough to flip it.

The `our`-store member classification now says whether a key is a direct
member or a sub-package (the same distinction the env scan already made). A
sub-package gets a placeholder only after the direct members are in, so the
real `tmp::Foo` entry wins, and the keyed lookup ignores sub-package keys.
CompUnit::Util's `set-in-WHO` builds nested stashes exactly this way (#17475).
