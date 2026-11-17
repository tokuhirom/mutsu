# Look-behinds match the way rakudo matches them

`<?after X>` / `<!after X>` used to search forward for a start of `X` that
ends at the current position. Rakudo instead flips `X`. It reverses each
concatenation and mirrors each anchor (`^^`/`$$`, `^`/`$`, `<<`/`>>`), then
runs the result forward over the reversed subject (#16031). Two visible
differences follow:

- Under `:ratchet` a look-behind's quantifiers are greedy *leftwards* and
  never give anything back. `rx:r/<?after ^^ \s* 'set'> '$'/` does not match
  `"a\nset$x"`, because `\s*` eats the newline and `^^` is then tested after
  the `a`.
- `^^` and `$$` keep their end-of-string exceptions on the mirrored side:
  `"ab\n" ~~ /ab \n <?after \n $$>/` matches.

A look-behind built only from direction-free atoms (literals, classes,
groups, alternations) and mirrored anchors now runs that way
(`regex_lookbehind_flip`). The reversed window is bounded by the pattern's
longest match. A look-behind that calls a rule keeps the forward search:
rakudo runs a called rule forward over the reversed subject, which is not
worth copying.

A look-around body also inherits the enclosing `:ratchet` now, as a nested
nibbler does in rakudo. Sway::Config's `t/01-sway-config.rakutest` passes
all 15 subtests as a result.
