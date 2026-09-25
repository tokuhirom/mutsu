# A failing literal regex search no longer calls `bcmp` per character

Closes [#9250](https://github.com/tokuhirom/mutsu/issues/9250).

After #9144 made per-call regex setup O(1), a failing unanchored search such as
`$s ~~ /b/` was pure scan cost, and that scan was slow. The ADR-0099 prefilter's
`Literal` arm, and the `find_from` helper behind its `Inner` arm, tested every
candidate position with `chars[i..i + n] == needle`. Comparing two `[char]`
slices compiles to a `bcmp` call, so a subject with no occurrence paid one
function call per character, about 3.4 ns each.

Both arms now share `find_literal` (`src/runtime/regex/regex_prefilter_find.rs`).
It finds the needle's first character 32 characters at a time using a
branch-free `fold` that LLVM vectorizes, and it compares the rest of the needle
only where that first character matched. The function works on `char`s, not
bytes, so it returns exactly the position the per-position compare did. That
includes `:i`/`:m` targets built with `MatchTarget::from_chars`. A unit test
checks it against the old compare across block boundaries, non-ASCII needles,
empty needles and every `from`/`last` bound. New cases in
`tests/regex_prefilter_differential.rs` put occurrences on either side of a
block edge.

Release build, N = 40000 calls on `"a" x 40000`, paired runs on a 4-core
container:

| | before | after | rakudo |
|---|---:|---:|---:|
| `$s ~~ /b/` | 2.65-3.00 s | 0.16-0.21 s | 0.12-0.14 s |
| `$s.contains(/b/)` | 2.7-3.0 s | 0.23 s | |
| `$s ~~ /\w b/` (inner literal) | 2.5 s | 0.17-0.19 s | |

That is about 15x faster, and within 1.5x of rakudo, well inside the issue's
4x goal. The regex path is now faster than mutsu's own `.index('b')`
(about 1.1 s on the same input), which still scans a character at a time.
