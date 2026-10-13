# `bless` on a Hash subclass no longer stores its named arguments as entries

On an `is Hash` (or `is Map`) subclass, `self.bless(:x(1))` stored `x => 1`
as a hash entry as well as passing it to `BUILD`/`TWEAK`. In Rakudo `bless`'s
named arguments only initialize attributes; the hash's own entries come from
`Hash.new`, which `bless` does not call. PDF's `PDF::COS::Dict.new` calls
`self.bless(:$dict, :$seen-lock, |c)`, so every Dict carried `dict` and
`seen-lock` keys and `t/cos-deref.t` failed at test 4 (#13087). `bless` now
keeps only positional arguments for the storage, and `Hash.new(a => 1)` on a
subclass still stores its pairs.
