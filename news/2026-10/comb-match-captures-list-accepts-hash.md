# `.comb($rx, :match)` keeps captures; a Hash smartmatches a list by its pairs

Path::Map's suite now passes all five of its test files; it used to pass none.

- **`.comb($regex, :match)`** returns whole Match objects, with named and
  positional captures. It used to build bare position-only Matches, so
  `$<var>:exists` was always False and `$<path>` was Nil. Path::Map's
  `add_handler` therefore built every route out of empty segments. The
  `:match` form now takes the capturing regex path.
- **`%h ~~ ()`**: `List.ACCEPTS` compares the topic's `.list`, and a Hash's
  `.list` is its pairs. A Hash topic used to count as non-iterable, so it
  never matched a list, and an empty Hash failed to match `()`.
