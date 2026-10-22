# An element store or push on an undefined scalar captured by a sub is kept

`my $lookup; my sub d($n) { $lookup{$n}.push(1) }` left `$lookup` undefined: the
element store vivified a Hash in a root the sub's frame did not read, and the
capture analysis ignored the write because it looked for `$`-prefixed names
while scalar names carry no sigil. `op_name_write_const_idx` now counts an
element store on any non-`@`/`%`/`&` name as a write of the captured variable,
and the keyed store rebinds the frame's env entry to the vivified (itemized)
container. A chained `$s<a><b> = 1` is still lost; that is #14647.
