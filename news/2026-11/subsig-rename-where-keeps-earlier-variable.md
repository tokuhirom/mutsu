# A `where` on a sub-signature rename sees the variables earlier parameters bound

`Ge (:$left), Ge (:left($l) where { $left eq $_ })`: while checking the second
`where`, mutsu bound the rename's KEY (`left`) to the candidate, shadowing the
`$left` the first sub-signature had bound, so the clause compared a value with
itself and always held. Binding and multi dispatch no longer bind the key of a
rename for the check. Found through Red's AND optimizer multis
(`GeGt $right (:left($rl) where { compare $ll, $rl }, ...)`).
