# An exported proto keeps its own multis after `add_dispatchee`

`BEGIN &proto.add_dispatchee(&other)` filed the new candidate row with no source
file, so a module's private `my proto`/`my multi` family saw it as the main
program's. The module's `sub EXPORT` then read the family without the dispatchee,
and the importer read the dispatchee without the module's own `multi`
(`JSON::Fast::Hyper`'s `to-json` serialised `<a b c>` as `["a","a","a"]`, #14500).
The row now carries the unit that adds it.
