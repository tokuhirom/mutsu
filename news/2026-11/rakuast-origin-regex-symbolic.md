# RakuAST: origins of a symbolic subrule call

`<::("w")>` and `<.::('digit')>` in a regex now have rakudo's ranges: the
`Assertion::Named`, its `WithWhitespace`, its `Name` (from the `::` to the
`)`) and the string that computes the name. Before, one such call left every
node of its regex without a range. Its `Name` also visits the string, as
rakudo's does (#15780). `::("w")` outside a regex still lacks the `Name` range.
