# A lexical class's bareword no longer rescans every type map per read

Reading the bareword of a `my class` / `my role` (declared in a role or
module and read from its methods) went through `resolve_lexical_type_key`,
whose miss path walks the four type maps looking for the `Name\0<id>` mangled
key (~6.4k instructions each, six reads per AttrX::Mooish iteration). The scan
reads the registry only, so its answer is now memoized per source-facing name
on the registry and dropped on every registry write, like the lexical-name
index beside it.

Part of #15688.
