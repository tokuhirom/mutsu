# Str methods now document their complexity, and the quadratic ones are tracked

Following the `nqp::` op audit (`news/2026-09/nqp-op-complexity-audit.md`),
every `Str` method implementation and every string operator (`~`, `~=`, `x`,
`eq`/`lt`/`leg`/`cmp`) now carries a `// Cost:` line. Where mutsu's bound is
worse than Rakudo's, the line adds a `Rakudo: O(..) -- see #NNNN` suffix. The
rules now live in one place, `docs/complexity-annotations.md`, and the `nqp::`
legend points to it. `grep -rn 'Rakudo: O(' src/` lists 65 deficit sites.

`scripts/str-complexity-check.sh` times 32 cases at N and 2N and prints the
ratio. Several control cases, which must stay linear, run alongside them.
Like the `nqp::` script, it is a manual diagnostic and not a CI gate.

## What the audit found

On a release build each of the following is quadratic in mutsu and linear in
Rakudo. The ratio is t(2N)/t(N).

- **#9140, positional methods.** The Str payload is copied on every call, and
  nothing caches a char or grapheme index, so each call is O(n). Affected:
  `.chars`, `.substr`, `.index`/`.rindex`/`.contains` with a position,
  `.indices`, `.ord`, `.starts-with`, `.substr-eq`. The ratio is 3.6 to 4.1,
  so "walk the string by index" loops are O(n²).
- **#9141, `~=` outside the fused local path.** Hash and array elements,
  attributes, `given`/`when` locals and `$y = $y ~ $x` all copy the whole
  string and re-run NFC on it for every append (ratio 3.8 to 4.1).
- **#9142, `.trans`.** A multi-char or regex key, including `"\n" =>
  "\r\n"`, is O(n²) in a single call (ratio 4.0 to 10.3).
- **#9143, `s:g///` and the `.subst` slow path.** It copies a MatchTarget per
  match and does an r² capture lookup. The regex slow path enumerates every
  match end at every start: `.subst(/a+/, {...})` ran out of memory at 10k
  chars.
- **#9144, regex entry points.** `~~ /rx/`, `.match(:c/:p)`,
  `.contains(rx)`, `.comb(rx, :match)` and `.prematch` copy the subject per
  call or per Match.
- **#9145, `.split` with several separators.** Each piece rescans every
  separator to the end.

Per-call overheads that do not change a loop's order, such as `Str.Str` and
`.WHICH` copies, comparisons copying both operands, and `lines`/`words`
limits that do not stop the scan, are grouped in #9147.

The audit also found two correctness bugs, filed as #9146:
`.contains(rx, $pos)` cannot see before `$pos`, and `.substr-eq` counts
codepoints where it should count graphemes.
