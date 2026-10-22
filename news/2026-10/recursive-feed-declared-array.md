# A recursive sub's `... ==> my @val` keeps one array per call

`@in ==> map { ... recurse ... } ==> my @val` declares `@val` in expression position, which compiles
with a per-site `__do_decl_init_N` marker slot ("initialize once per declaration site"). A frame
seeds its slots from the env by name on entry, and once any code in the program used a symbolic
lookup (`$::("X")`, `::($name)`, `EVAL`), every slot is mirrored into the env. The recursive
callee then found its caller's marker, skipped its own declaration and fed the caller's array.
The marker is now a fresh local: an activation never starts with another activation's marker.
`Date::Calendar::Strftime`'s `%F` (the module's `_strftime` calls itself) expanded twice because of
this; its `t/03-method.rakutest` and `t/08-sub.rakutest` now match raku (#14498). The loop-body
reuse of the same marker is filed as #14626.
