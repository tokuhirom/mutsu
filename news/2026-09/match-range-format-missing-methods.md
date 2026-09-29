# Match, Range, and Format method coverage

Implemented `Match.replace-with` using the original subject and match span, including Unicode text and failed matches. `Range.in-range` now accepts a custom error label, and reversing an infinite range raises `X::Cannot::Lazy`. `Format.directives` exposes the conversion directives parsed from its format string, including dynamic width and precision arguments.
