unit module JSON::Tiny;

# A stand-in JSON::Tiny placed on the module-resolution ladder, used by
# t/modules/batteries/json-module-ladder.t to prove that an explicit `use lib`
# / `-I` / `MUTSULIB` copy outranks the bundled battery in modules/JSON-Tiny/
# (BATTERIES.md §6). mutsu used to answer the bare name `JSON::Tiny` from a
# native Rust implementation that jumped the whole ladder, so neither this nor
# the bundled module could be reached (#8183).

sub to-json($d) is export { "ladder-to-json:" ~ $d.join(",") }
sub from-json(Str $text) is export { "ladder-from-json:$text" }
