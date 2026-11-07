# RakuAST nodes collect check-time problems

`RakuAST::Node` answers `sorries`, `worries`, `has-check-time-problems` and takes `add-sorry`,
`add-worry`, `clear-worries` and `promote-worries-to-sorries`, as rakudo 2026.09 does (lists of
exceptions, empty for a fresh node; promoting appends the worries to the sorries). The lists are
hidden from `.raku`, rendering and walks.
