# An interpolated `$( … )` regex is matched lazily and keeps no captures

The pattern a `$( … )` / `@( … )` interpolation yields used to be matched for every possible end
up front, so a code block inside it ran at each of them:
`my $r = rx/ a+ { $n++ } /; "aaab" ~~ / $($r) b /` ran the block three times where rakudo runs it
once. The compiled regex engine now runs the yielded regex as a frame that is resumed only when
the match backtracks into it, as rakudo does. The interpolated regex's own captures are no longer
kept either; rakudo drops them too. This removes the `code-interp` bridge, the largest
remaining use of the tree walk (ADR-0135 Slice E).
