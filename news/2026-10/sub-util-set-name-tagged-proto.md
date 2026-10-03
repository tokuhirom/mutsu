# `.set_name` keeps a qualified name; tag-only exported protos stay unimported

Sub::Util's `set_subname` renames a block to `GLOBAL::foo` or `Foo::foo` and
its test reads the name back. mutsu stored the new name but `.name` stripped
everything up to the last `::`, so it answered `foo`. `.name` now returns the
stored name as is; a sub declared in a package was already stored under its
bare name, so `&P::q.name` is still `q`.

The same distribution checks that `use Sub::Util;` imports nothing: its
`set_subname` is an `our proto sub ... is export(:SUPPORTED)`. mutsu
registered every exported proto under `GLOBAL::` at declaration, whatever its
tags, so `::('&set_subname')` was defined without any import. That alias now
stands only for a default (`is export` / `:DEFAULT` / `:MANDATORY`) export; a
proto under another tag is installed by the `use` that names the tag.

All three Sub::Util test files now pass (one of three did before).
