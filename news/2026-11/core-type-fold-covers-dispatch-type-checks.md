# The CORE type fold covers a dispatcher's type checks

ADR-0115 folds a CORE type name used as an `nqp::` operand to its type object at parse time,
for a fixed list of types. The list held the container and string types JSON::Fast's decoder
names. Its encoder's `jsonify` asks `nqp::istype($_, T)` for each value, for up to fifteen
types in turn: `Bool`, `IntStr`, `RatStr`, `NumStr`, `Enumeration`, `FatRat`, `Rational`,
`Associative`, `Positional`, `Dateish`, `Instant`, `Real`, `Version`. None of those were on the
list, so every check resolved its type name through the full run-time bareword chain.

The list now includes those types, along with the other common CORE classes and roles a type
dispatch names (`Numeric`, `Iterable`, `Callable`, `Date`, `DateTime`, `Set`, `Bag`, ...), 56
names in all. It is kept sorted and binary-searched. A new test checks every folded name
against forty kinds of value, with the expected answers taken from rakudo.

A `jsonify`-shaped dispatcher writing a string went from 49.3k to 46.6k instructions per call
(callgrind, release). That is a bounded saving, not a structural one.
