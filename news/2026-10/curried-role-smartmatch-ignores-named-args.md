# A curried role is smartmatched by its positional arguments

`$w ~~ NT[Int, :coerce(&cf)]` was False for an instance of `NT[Int, :coerce(&cf)]` itself, and a
plain `NT[Int].new` did not match it either. The named arguments of a curried role are no part of
the type it names in rakudo, so the smartmatch against a parametric role now compares the
positional arguments only (#15710).
