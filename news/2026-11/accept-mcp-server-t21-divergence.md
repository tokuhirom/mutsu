# MCP::Server t/21 is an accepted divergence

MCP::Server's `t/21-elicitation-dispatch.rakutest` subtest "A round that answers only some of the
questions" assumes that both of a handler's `start` blocks ask their question before the
dispatcher snapshots the round, and that the first block gets the first key. Rakudo passes it
only on a cold thread pool, where one worker runs everything in FIFO order: re-measured on
2026-11-19, raku passes 25/25 cold but 13/25 after a pool warm-up, failing the same subtest in
the same two ways mutsu does (mutsu: 6/25 cold, 18/25 warm). ADR-15967 already decided that no
language rule orders two tasks' bodies and that mutsu does not copy rakudo's pool state.

Until now the whole 0.7.2 release was excluded from the ecosystem sweep for this one file. On
the user's decision the exclusion is replaced by an `ecosystem/accepted-divergences.toml` entry
pinned to mutsu's exact failure shape (ADR-0130), so the other 23 files stay measured and
MCP::Server reads `green` instead of `partial` (ADR-15967 §10, #17724, #14684).
