# A role method called on a `pun but Role` type object from another thread keeps the mixin as `self`

A method of a parametric role called on `R[Int].^pun but M` from a thread that did not create the pun
ran with `self` bound to the bare `R[Int]` (so `self.new` built an instance without `M`), because the
mixin dispatch could not find the pun class the creating thread had composed. The method-call
funnel now composes the pun from its published recipe (ADR-11456) before dispatching on a mixin
whose base type object is a foreign pun. Part of #17149 (Monad's cached `pun but Some` types).
