# `Int.new` coerces its argument as `.Int` does

rakudo's `Int.new(value)` is `value.Int`. A numeric string parses the way Raku
reads one (`Int.new("0x1F")` is 31), and one that is not a number dies with
the `X::Str::Numeric` from `"x".Int`. mutsu ran the argument through a lossy
integer conversion, so `Int.new("x")` and `Int.new("0x1F")` both answered `0`.
A type-object argument now dies with rakudo's `Cannot create an Int from a 'Any'
type object`.
