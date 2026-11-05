# A `for` / `while` body's `my $*OUT` no longer outlives the body

`for 1 { my $*OUT = $*ERR; ... }` left `$*OUT` pointing at the body's handle after the loop. The
loop body's shadow restore put back only the `*OUT` env key, not its `$*OUT` twin that `say` /
`put` / `print` read. The restore (and the removal of a body-local that shadowed nothing) now
handles both spellings. Closes #16193.
