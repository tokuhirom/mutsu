# `.origin` of sigilless declarations

A sigilless declaration (`my \S = 1`, `my \x := 2`, `my Str:D \s = "a"`) now
carries the ranges rakudo 2026.11 gives it:
- its `Name` covers the identifier after the backslash;
- its type covers the text before the backslash;
- its `Initializer` runs from the `=` or `:=` to the end of the value.

The name and type are placed by the new `sigilless_origins` module, and the
initializer through the reader variable declarations already use (#15780).
