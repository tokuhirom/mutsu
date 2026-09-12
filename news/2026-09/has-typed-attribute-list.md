# Typed parenthesised `has` declarations now parse

mutsu now accepts Raku's `has Type ($.first, $.second)` form and applies the
common type constraint to every declared attribute. This also covers the
`int32` list form used by NativeCall CStruct declarations.

The declarations are lowered through the existing grouped-attribute path, so
plain classes and CStruct field layouts now agree with Rakudo instead of
evaluating the attribute twigils as an expression without a `self`.
