# A `&*name` parameter is visible to the routines it calls

`method map($object, &*JSON-PATH-MAP)` binds a dynamic Callable that callees
read as `&*JSON-PATH-MAP(...)`. The parameter lived only in its frame's local
slot (callees found nothing), and a `gather` body forced further up inherited
the puller's `$*x` dynamics but not its `&*x`/`@*x`/`%*x` ones. A `&*` parameter
slot is now always mirrored into the env, and a gather body inherits every
sigil's dynamics. Found with JSON::Path, whose `t/04-map.rakutest` now matches
raku.
