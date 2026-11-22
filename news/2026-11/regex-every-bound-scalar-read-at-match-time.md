# Every bound `$var` in a regex is read when the cursor reaches it

ADR-12917 makes a regex `$var` a match-time atom: rakudo reads the variable
when the matcher reaches it, so a code block earlier in the pattern can change
what the atom matches. mutsu had moved most shapes there in earlier slices of
#14510. Several still went through the interpolation pre-pass, which pasted the
value into the pattern text before matching:

- a package-qualified scalar (`$P::v`);
- the variable of a `$foo=ATOM` alias;
- a placeholder (`$^a`);
- a callable or type-object value;
- `$var` after `<<`, which the scanner misread as a `<$var>` assertion;
- any pattern parsed outside the match entry points: a `<$var>` source, a
  `Regex` value's own text, a sub-pattern, a grammar candidate.

Now `parse_regex` and `parse_regex_uncached` themselves hoist every bound
scalar into a match-time `VarInterp`. The pre-pass splices only constants,
which rakudo also inlines at compile time, `"..."` literal text, and names
that no scope binds.

Three answers changed to match rakudo:

- `/ x { $P::v = "b" } $P::v /` and `/ x { $x = "b" } $x=[ y ] /` now match
  `"xb"`;
- `my $x := Nil; "a" ~~ / a $x /` is now `False`. A `Nil` or type-object
  value matches nothing.
