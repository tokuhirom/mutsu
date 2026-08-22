# Separate nominal `.isa` checks from role composition

`.isa(Type)` now follows only the nominal class hierarchy, matching Raku's
`.^mro` semantics. Values that compose roles such as `Numeric`, `Real`,
`Rational`, `Stringy`, `Positional`, `Associative`, `Iterable`, `Callable`,
and `Dateish` no longer report that role through `.isa`.

Role-aware operations remain intact: `.does(Type)`, smartmatch, and type
constraints continue to recognize those compositions. Concrete nominal
relationships such as `Array isa List`, `Hash isa Map`, and
`Sub isa Block/Routine/Code` are retained. The regression test also pins the
distinction so future type-hierarchy work cannot conflate roles with nominal
supertypes again.
