# A `sub EXPORT` hook's qualified type keys no longer read as `Any` in a gather-holding method

A module routine that holds a `gather` keeps the calling frame's env. When the
importing unit's `use` took a `sub EXPORT` hook that returned package-qualified
keys (`'Graph::Indexed' => Graph::Indexed`), that env still held the
declaration seed (`Package(Any)`) of each key, because the hook value itself
goes to the importer's lexical import set. The routine's bareword
`Graph::Indexed` resolved to that seed, so `Graph::Indexed.new` died with
`Default constructor for 'Any' only takes named arguments`. A package-qualified
term key holding only the seed is now ignored, and the name resolves to its
type. The `Graph` distribution's `t/01-basic-usage.rakutest` now matches
rakudo (50/50).
