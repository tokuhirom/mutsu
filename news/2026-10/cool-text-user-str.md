# Cool text methods read an `is Cool` instance through its own Str

`flip`, `uc`, `trim`, `comb` and the other `Cool` text methods are `self.Str.METHOD`. For an
instance whose class declares a `Str` method, the shapeless-`Cool` step of the method table now
calls it and hands the resulting string to the row, instead of the built-in `C<id>` form
(#12848).
