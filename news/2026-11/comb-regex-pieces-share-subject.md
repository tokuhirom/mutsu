# `.comb(rx)` pieces share the invocant's buffer

`.comb(/ <[a..z]>+ /)` turned every matched span into a fresh `String`: two
allocations per piece (the buffer and the `StrBody` holding it) plus a copy.
Each piece is now a `StrBody::slice_of` the invocant, the representation
`split` and `substr` pieces already use: one allocation and no copy. A slice
keeps its base alive only under `slice_of`'s existing rule (a base up to
64 KB, or a piece of at least an eighth of it); anything else is still copied.
The regex walk also stopped decoding the subject into chars twice (the
`MatchTarget` already holds them).

On the `comb` line of `bench-regex-global.raku` (480 pieces per operation;
`profiling` build, `MUTSU_JIT=off`, callgrind): **1.37M -> 1.14M instructions
(-17%)**, **985 -> 502 allocations per operation (-49%)**, 6.5 -> 5.9 ms locally
for 45 operations. Raku++ v5.9.0 takes 1.7 ms on the same line, so this line is
still the regex row's largest gap (#19296).
