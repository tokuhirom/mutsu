# `Range.chars` and `Seq.chars`

`(1..3).chars` died with "No such method 'chars' for invocant of type 'Range'". A Range
and a Seq are `Cool`, so `.chars` is the length of their string form (`"1..3".chars` is 5),
as for `IO::Path`, which already had it. Found with Format::Lisp, whose `~a` directive pads a
value using `.chars`: every test file of the distribution now matches rakudo.
