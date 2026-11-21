# `.origin` of `<[+-]>` enumerations and anonymous tokens

Two regex forms ended the origin walk early and now match rakudo 2026.11:

- **Character enumerations.** An enumeration holding a literal `+`, `-` or
  `.` (`<[+-]>`) or a backslash-escaped character (`<[\-.]>`) used to stop the
  walk, so it and every regex atom after it were left without a range. These
  elements are now read as written.
- **Anonymous declarations.** An anonymous `token { ... }`, `rule { ... }` or
  `regex { ... }` used as a value placed nothing inside its body. The
  declaration reader required a name; it now accepts the anonymous form, so
  the body is placed as in a named declaration.

(#15780)
