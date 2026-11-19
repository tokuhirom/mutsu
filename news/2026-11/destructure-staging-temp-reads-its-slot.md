# `my ($a, $b) = f()` reads its staging array through a slot

A list-assigning declaration is lowered to a block. That block stages the
right-hand side in a hidden array, `@__destructure_tmp__`, and each target
then reads its element back. The temp's `__` name does not look like a plain
user lexical, so the compiler read it by name, walking the environment once
per target. That by-name read also forced the temp's own store to be mirrored
into the environment.

Only the declaration's own block ever reads the temp, so its slot is its only
home. The compiler now admits exactly this staging temp to slot reads.

A `my ($name, $next) = f($x)` declaration costs 37.4k instructions instead of
39.9k (callgrind slope, profiling build). MCP::Server::Tool::Web's tokenizer
does this once per tag and once per attribute, and its slope dropped by 0.5%.
The remaining cost of the shape is the staging array's own declaration store,
the seed declarations of the targets, and the element reads. Together they
still make it about 15k instructions dearer than the hand-written
`my $l = f($x); my $name = $l[0]; my $next = $l[1]`. The work is part of
#16897.
