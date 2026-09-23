# A zero-width atom under `**N` counts its iterations

`"xx" ~~ /<?before x>**2/` failed in mutsu (Rakudo: matches), and so did
`**1`, `**2..3`, `[<?before x>]**2` and the capturing `<before x>**2`.

Every quantifier loop in the regex engine broke on the first iteration that
matched zero-width, **without counting it** -- the usual guard against `*`
looping forever at one position. That is harmless for `*`, but it left a
counted quantifier short of its minimum, so the whole match failed.

Rakudo has no such guard at all: every iteration counts. A bounded quantifier
therefore repeats a zero-width atom up to its maximum (`<before x>**3..5 x`
records five `<before>` captures), and an unbounded one (`<?before x>+`) never
terminates. mutsu now follows Rakudo wherever Rakudo terminates, and keeps its
guard where Rakudo would not:

- a **bounded** quantifier counts a zero-width iteration up to its maximum;
- an **unbounded** one counts it only while it is below the minimum, which is
  enough to satisfy `+`.

The rule lives in one place, `src/runtime/regex/regex_zero_width_iter.rs`, and
every loop that had the bare break uses it: the general chain
(`grow_one_iter` / `walk_quant_chain`), the candidate-backtracking group walk,
the alternation walk, the ratcheted `*`/`+` fast paths, and the position-only
`**` scans in `regex_match_atom_simple.rs` and `regex_match_nocap.rs`.

Fixing it exposed a second, independent bug on the issue's own repro line.
`.so` and `.not` on a Match were delegated to the matched string, so every
successful zero-width match (`("yy" ~~ /x?/).so`) answered `False`, even though
`.Bool` was right. Both now answer from the Match itself.

The issue's third row, `$<a>=<?before x>**2`, still fails. The cause is a
separate bug: a sigil alias on a literal with an exact unspaced count
(`$<a>=x**2`) never matches, zero-width or not. It is filed as #9198.

Pinned by `t/regex/syntax/regex-zero-width-counted-quantifier.t` (#9180).
