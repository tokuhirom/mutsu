# An imported proto family's lookups are remembered per compunit

A call of a routine whose `multi`/`proto` family was imported from a module
asked two questions about the proto on every call: whether one exists, and,
for a `{*}`-only proto, which definition it is. Both walk the bare-name search
packages. The existing `has_proto` cache could not answer them for an imported
family, because whether such a family is visible depends on which compunit is
asking (#11004), and that cache's key did not say. So every call of
`Array::Sorted::Util`'s `inserts`/`finds`/`insert` paid the walk: about 4.8k
instructions for each `has_proto` and 4k for each proto resolution, several
times per call.

The new memo (`src/runtime/proto_resolve_memo.rs`) keys both answers by the
search context, the name and, for an imported family, the units that decide
visibility. The registry write generation covers it, and that generation moves
on every registry write.

The same profile showed a second cost. A type check against a module's
`my class` called `type_name_binding` on the class's NUL-mangled storage name,
and the term stores missed every time after walking the package chain (~17k
instructions). A NUL cannot appear in a source identifier, so a storage name
is never a constant's or a term's name. Only `env` is now asked for it.

On a `Concurrent::PriorityQueue` push in `t/02-concurrent`'s key order, the
instruction count per push fell from 1.057M to 0.943M (-10.7%, callgrind
slope, profiling build). This is a slice of #15786; the per-push cost is
still several times rakudo's.
