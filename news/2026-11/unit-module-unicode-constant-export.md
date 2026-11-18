# A module's constant named with a non-ASCII letter is imported

Math::Sequences::Real exports `my constant \ℝ is export = Reals.new`, and its
test read `ℝ.elems` as `1` and `ℝ.of` as an error: `ℝ` was `Any` in the
importing unit. The export stash had the name, but the passes that collect a
`unit module`'s lexicals for export and import only took names starting with
an ASCII letter or `_`, so `ℝ`, `Ñ` and every other identifier with a
non-ASCII initial was left behind. They now take any alphabetic initial, as
Raku identifiers allow, and Math::Sequences' t/Reals.rakutest matches rakudo.
