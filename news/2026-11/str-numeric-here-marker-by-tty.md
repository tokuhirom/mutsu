# `X::Str::Numeric` spells its position marker `<HERE>` unless stderr is a terminal

rakudo writes `... in '<HERE>x' (indicated by <HERE>)` when its error stream is
not a terminal and the coloured `⏏` form only on one; mutsu always wrote
`⏏`. Both `.message` builders now share one `build_source_indicator` that
checks stderr, so scripts run under a pipe, a file or a test harness see the
same text as rakudo. Found by the raku-doc differential
(`Type/Enumeration.rakudoc`), #16532.
