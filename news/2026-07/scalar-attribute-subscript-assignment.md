# Subscript assignment through a `$`-sigil attribute reaches the instance

`$!h<k> = 1` and `$!a[0] = 2`, where `$!h`/`$!a` is a scalar attribute holding a
Hash or Array, used to be dropped silently. The `%!`/`@!` sigil forms worked, and
so did the same write to a lexical `$` holding a Hash, which is what made the
failure so easy to mistake for a container-subclass or type-constraint problem.

The cause was a gap between the two halves of the attribute machinery. A scalar
attribute is *cell-direct*: `self`'s shared attribute cell is the source of
truth, and the method frame's local slot is populated lazily, by the first
cell-direct **read** of that attribute. The element-assignment ops, though, are
env-centric — they look the container up by name in env. So when a method's first
touch of the attribute was an element write, there was nothing in env or in the
slot to find, and the op autovivified a fresh container that never reached the
cell. Any preceding read of `$!h` — even `my $t = $!h` — seeded the slot and made
the very same write work, which is why the bug looked intermittent.

The two helpers that already keep an attribute's env copy and its cell in sync
around a container-mutating op (`attr_env_snapshot` / `mirror_attr_env_to_cell`,
formerly named for the array/hash case) now cover scalar attribute twigils as
well: the snapshot refreshes env and the slot from the live cell before the op,
and the mirror writes the post-mutation container back if the op replaced it.

Fixing that exposed that the rest of the subscript-op family had never been
wired to those helpers at all — `IndexAssignExprNested`, `IndexAssignDeepNested`,
`IndexElemAutoviv` and `PostIncrementIndex`/`PostDecrementIndex` were missing
them, so nested and read-modify-write element updates were lost for the `%!`/`@!`
attributes too (`%!hh<a><b> = 3`, `%!hh<c>++`, `@!aa[0][1] = 5` all silently did
nothing). They are hooked up now.

This was the blocker behind `Template::Mustache`'s `06-logging.rakutest`, whose
`Logger` keeps its log routines in `has LoggersMap $.routines` (a `Hash`
subclass) and fills it from `BUILD` with `$!routines{.key} ||= …`; the map came
out empty, so replacing `routines<Warn>` with `&die` had no effect. The construct
is ordinary Raku, so the blast radius was much wider than that one module.

Pin: `t/attr-subscript-assignment.t`.
