# `.new` runs a user `bless`, and `self.Mu::bless` works on an instance

`Mu.new` is `self.bless(|%attrinit)`, but mutsu's native constructor never looked for a `bless` the
class (or a composed role) declares, so ValueTypeCache's role-level `method bless` cache was
bypassed (`Point.new.WHICH` was `Any`). `.new` now takes the full constructor and runs the user
`bless` when the class has one. `self.Mu::bless(|%_)` on an existing instance used to build a bare
`Mu`; it now blesses the receiver's class. ValueTypeCache's `t/01-basic.rakutest` is 2/2, like raku.
