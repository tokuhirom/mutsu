# RakuAST: origins of a method call on a continuation line

`my $q = "abc"\n    .uc.lc` is an `ApplyDottyInfix(left, DottyInfix::Call, Call::Method)`. Only the `.=`
form was placed; the plain `.` form now gets what rakudo gives it: the `Call` over the dot, the
`Method` and its `Name` over the name, the `ArgList` over the arguments (empty right after the
name when there are none), with blanks, newlines and comments allowed before the dot (#15780).
