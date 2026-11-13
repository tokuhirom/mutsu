# A rebound attribute is seen by the frame that read it first

A method frame seeds each `:=`-bound scalar attribute it reads with the bound
container. When a nested method rebound that attribute (`method nx { $!cur :=
...; self }`, called as `self.nx`), the calling frame kept reading the old
container, so it saw the previous value -- and an iterator that advanced
through such a method looped on its exhausted first part, or stopped early.
The read now follows the instance's current binding whenever it differs from
the frame's seed; a bind made in the frame itself, and assignments through a
binding, are unaffected.

`ForwardIterables`' `t/01-basic.rakutest` now passes 7/7, matching raku.
