# `$proc.stdout.lines.tap` receives lines

A tap on `Proc::Async`'s `stdout.lines` (or `stderr.lines`) taken before
`.start` got nothing and an immediate `done`: the `.lines` view was registered
under an id the process never feeds. The view is now tapped on the process's own
stream with a per-tap line mode, so the child's output arrives line by line as it
is written (and by replay when the process has already run), with `:!chomp`
honoured and a raw tap beside it still seeing whole chunks. MCP::Client's
stdio transport reads its server this way: `t/09-stdio-failures.rakutest` passes
now and `t/08-stdio-roundtrip.rakutest` answers its first two subtests.
