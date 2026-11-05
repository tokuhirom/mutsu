# `my %h is Role` binds the role's type-capture defaults

`my %m is Hash::Sorted` punned the role without binding its defaulted type captures
(`role Hash::Sorted[::KeyT = str, ::ValueT = Any]`), so `new` saw the `%` variable's key
type (`Str`) as `KeyT` and built an `Array[KeyT]` that rejected its own keys. The tie now
punns the role with its defaults bound, as `Role.new` does. Hash::Sorted's `t/01-basic`
passes 21/21 (it died before its first assertion).
