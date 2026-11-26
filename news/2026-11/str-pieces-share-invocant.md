# `comb`, `lines`, `words`, `substr` and Match text share the invocant's buffer

`StrBody::Slice` (added for `.split`, #19201) now also backs:

- every element `.comb`, `.lines` and `.words` cut (`str_iter.rs`, one
  `pull_one`);
- both `.substr` paths: the native fast path and the interpreter's
  `dispatch_substr`, including the `Range` and `WhateverCode` forms;
- the matched text of a Match, and of a quantified capture in a substitution,
  when the subject is ASCII (char index == byte index). A non-ASCII subject
  still re-encodes its span.

Each element is now one allocation, its `Str`, instead of a copy of its text
plus that `Str`.

A slice may now also rest on a strand list that has already been flattened,
because reading one is O(1) through its cache. That matters because
`"…" x $n` and `~` build strand lists. Without it, a `lines` or `words` of
such a string would have copied every piece. A strand list not yet flattened
is still copied from, so a slice never pays a flatten on its first read.

Measured with callgrind (`MUTSU_JIT=off`) against `main` at `afd0efb7`:
exactly one allocation fewer per element in each case, and 5-9% fewer
instructions for `words`, `lines` and `comb`.

| loop | elements | allocations | instructions |
|---|---:|---|---|
| `for $text.words` | 9,000 | 29,812 → 20,810 | 45.4M → 41.8M |
| `for $text.lines` | 10,000 | 34,871 → 24,871 | 52.8M → 50.3M |
| `for $text.comb` | 11,000 | 33,555 → 22,557 | 55.5M → 50.6M |
| `$text.substr($i, 5)` | 20,000 | 71,550 → 51,549 | 142.1M → 141.7M |
| `$text.match(/\w+/, :g)` + `.Str` | 90,000 | 1,197,879 → 1,108,090 | 1,588M → 1,559M |
