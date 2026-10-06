# A `CArray:D` parameter binds through an imported constant alias

Upstream NativeCall exports `my constant CArray = NativeCall::Types::CArray`. The bare name
`CArray` is on mutsu's known-type list (the native provider's), so the constant-alias walk skipped
it, and the bare-`CArray` shortcut compared the instance's qualified class name
(`UNC::Types::CArray[uint8]`) against the unqualified spelling. `CArray:D $x`, `CArray:U $x` and
`$a ~~ CArray:D` therefore rejected a `CArray[T]` instance, while the qualified spelling and the bare
`CArray $x` parameter bound. The shortcut now follows the alias first, like any other constant type
alias (#12121).
