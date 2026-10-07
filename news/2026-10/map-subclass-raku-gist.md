# `.raku` and `.gist` of a `Map` subclass name the subclass

`class V3 is Map {}` instances rendered as `Map.new(...)` (`.raku`) or as a plain
hash `{a => 1}` (`.gist`). They now render as rakudo does: `V3.new((:a(1)))`,
`V3.new((a => 1))`, and `V3.new(())` for an empty one. The rename is one shared
helper (`rename_map_subclass_repr`) applied at the pure gist/raku renderers and at
the instance-delegation paths; an `is Hash` subclass is unchanged. Closes #12170.
