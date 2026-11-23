# One Roast figure, counted against spectest.data

README, the site and `HISTORY.tsv` used to count Roast against three different
file sets: 1,437 / 1,455, 1,426 / 1,454 and 1,269 / 1,288. None of them was
the set Rakudo runs. All three now report one figure, from one generator,
`scripts/roast-compat.py`. It counts the files of `roast/spectest.data`,
Rakudo's run list, with the `#?rakudo` fudge applied, which is how Raku++
counts. mutsu passes **1,425 of the 1,425**.

`HISTORY.tsv` keeps its old columns, so its series stays comparable, and gains
`spec_files` / `spec_pass` columns, filled from the next full run on.
`HISTORY-pass.svg` marks where that series starts. The definition is in
`docs/roast-compatibility.md`. (#19036)
