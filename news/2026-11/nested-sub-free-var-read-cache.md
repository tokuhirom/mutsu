# A routine-nested sub reads its free variables without the by-name walk

A `my sub` declared inside a routine reads the declaring call's lexicals
(mutsu#9111). Every read went through the whole by-name resolution chain before
it reached the sub's alias cell: about 2k instructions per read. A character
scanner like MCP::Server::Tool::Web's tokenizer reads two such variables per
character.

The first read of each such variable in a call now records the binding it
resolved to, keyed by the call's frame, and later reads in the same call load
it directly. A binding is only recorded when every probe ahead of the alias
provably declined. The probes whose answer can change during a call (an
rw-return context, an atomic variable, a sigilless attribute alias, an
escaping `our` capture, a running `for` parameter of the same name) are
re-tested on every cached read.

`$probe++ while $probe < $len` in a nested sub, with `$len` the enclosing
routine's, went from ~4,050 to ~2,290 instructions per iteration (a local
`$len` costs ~1,920), and the tokenizer's `read-name` loop from ~16,700 to
~13,400.
