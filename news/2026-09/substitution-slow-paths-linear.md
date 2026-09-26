# `s:g///` and the `.subst` slow paths are linear

#8247 made `.split(/rx/)` and the plain `.subst(rx, :g)` fast path linear.
Three other substitution paths were still super-linear. Rakudo is O(n + r) on
all three, and now mutsu is too (#9143).

## What was quadratic

- **`s:g///` / `S:g///`** (`src/vm/vm_subst_exec.rs`, `vm_subst_apply.rs`):
  - Each selected match found its captures with a linear `find` over all
    matches, which is O(r²).
  - Every `$/` Match built its own `MatchTarget`, which copies the subject and
    its chars. `$/` then held r copies of the subject.
  - Every span went through `char_idx_to_byte`, which counts from the start of
    the string.

  `s:g/","/;/` on `"a," x 20000` took 21 s, and at 40000 it ran out of memory.
- **`.subst` regex slow path** (a closure replacement, `:nth`, `:x`, `:c`,
  `:p`): `regex_match_all_with_captures` listed every possible match end at
  every start and then kept the longest match per start. It did this even for
  a single, non-`:g` call. `("a" x 5000).subst(/a+/, { "b" })` took 28.6 s.
- **`.subst` literal slow path**: each match counted
  `text[..start].chars().count()` from the start of the string, and a closure
  replacement built a fresh `MatchTarget` for every match.

## The fix

- One `MatchTarget` per call, shared by the scan and by every `$/` Match.
- The regex slow path now runs one leftmost scan (`subst_scan_matches`), like
  `m:g` and the fast path, starting at `:c`/`:p`. It stops once the adverbs
  have all the matches they can use: one without `:g`, the largest `:nth`
  index, or the upper bound of `:x`. A call with no adverbs goes straight to
  the single-match search.
- `s///` pairs the selected ranges with their captures in one forward pass.
- A new `CharByteCursor` (`src/runtime/utils/char_cursor.rs`) converts
  ascending char offsets to byte offsets in one pass. It replaces
  `char_idx_to_byte`, which had no callers left.
- `:nth` de-duplication compares against the last entry (the list is validated
  ascending) instead of calling `contains`.

`scripts/str-complexity-check.sh` ratios (time at 2N divided by time at N,
where ~2 is linear) went from 3.81 / 4.45 / 4.75 to 2.02 / 1.08 / 1.98. They
stay ~2 at 20× the base N. `s:g///` over 40000 matches now takes 0.12 s.

## Behaviour fixes that came with it

Because the regex slow path now scans the way Rakudo does:

- `"aaaa".subst(/a+/, "b", :c(2))` is `aab`. Before, it matched the whole
  string from 0 and then dropped that match because it started before 2.
- `"aXbXcX".subst(/.*?X/, {...}, :g)` finds three matches, not one greedy one.
- A `{ ... }` block in the pattern no longer runs for match ends the call
  throws away.

Pinned by `t/regex/subst/subst-slow-path-linear.t`.
