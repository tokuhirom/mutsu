# CSS::TagSet 0.1.4: all three test files pass

The `CSS::TagSet` distribution's own suite (`t/tag-set-xhtml.t`, `-pdf.t`, `-pango.t`) now passes
under mutsu, as it does under rakudo ([#10310](https://github.com/tokuhirom/mutsu/issues/10310)).
Getting there took seven general interpreter fixes, each with its own regression test:

- `CArray[T]` is `Positional`: `grep`/`map`/`first`/`tail` and a non-itemized `for` walk its
  elements instead of treating the array as one item.
- A `my $x` in a nested block that shadows an enclosing `for -> $x` parameter no longer overwrites
  the parameter after the block.
- `$!attr := $_` in a `TWEAK` left an alias to the frame's topic that was merged into the caller,
  clobbering the caller's `$_` in later `.&sub` calls.
- `.can` on a value mixed with one candidate of a role group looks in that candidate.
- An untyped positional ranks as `Any` among method candidates, so a `where` refinement on another
  parameter wins as it does in rakudo.
- A named parameter's trailing sub-signature (`:at-rule(%)! (Str:D :at-keyw($type)!, *%ast)`) is
  bound, and takes part in candidate matching.
- The `flat` routine reads deferred `Seq`s nested in its operand.

The two symptoms the issue named (`$*USAGE` reading `Nil`, `CSS::Properties.clone` rejecting an
undefined `reference-width`) no longer reproduce on this build.
