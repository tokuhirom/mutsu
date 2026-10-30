# An `@!x = ...` / `%!x = ...` store resolves its attribute once

An `@`/`%` attribute has no local slot, so `%!children = %ch` reaches the VM as
`SetGlobal("%!children")`. Each execution looked the attribute up by name four
times (the tied-container probe, the coercion's current-container read, the
hash-key check, the element-type check), once more to write the cell, and then
mirrored the value into the frame env, which made the method return run a
`merge_method_env` pass that only dropped the key again.

The chunk's attribute inline cache now holds one word per constant after the
local-slot words, so the store reads and writes the cell through the same
`(layout, slot)` answer the `$!x` lane uses. The store reads the cell and the
memoized declared type once and hands them to the helpers, takes the name
symbol from the constant pool, and no longer grows an env mirror in a frame that
has none (the cell is the store of record).

Measured with callgrind on the `profiling` build, second run, 2000 calls of
`method set(%ch) { %!children = %ch; self }` over `method empty(%ch) { self }`
(same binary, A/B from the same `main`):

| | Ir per call over the empty body |
| --- | ---: |
| before | 18.2k |
| after | 11.75k |

This is a slice of #15444 (close condition: at most +10k).
