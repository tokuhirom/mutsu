# A hash with a role mixed in reads `Int` keys and keeps its type name

`%h{42}` on a hash a role was mixed into (`my %h does R`) was read as a
positional index and answered an out-of-range Failure; it is now an `AT-KEY`
lookup. `.^name` of a typed hash with a role (`my %h{Str} does R`) now reports
`Hash[Any,Str]+{R}` as rakudo does instead of `Hash+{R}`. Found with
WriteOnceHash (#15376).
