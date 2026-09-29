# `callframe(N).code.name` for method and mainline frames

A `callframe(N)` that landed on a method body or on the unit mainline had a Nil `code`, so
`.code.name` was empty. The callframe entry now remembers the routine frame it was pushed from
and reports `bar` for a method and `<unit>` for the mainline, as Rakudo does. Found by
Log::Async `t/14-frame.rakutest` (3/6 -> 6/6); `t/04-filter` still has one failing assertion.
