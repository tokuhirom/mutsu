# RakuAST::Name::Part::Empty removed

mutsu kept `RakuAST::Name::Part::Empty`, rakudo 2026.09's spelling of the empty
edge of a name, beside rakudo 2026.11's `RakuAST::Name::Part::EmptyEdge`.
Rakudo 2026.11 has only `EmptyEdge`, so mutsu now drops the old class too
(maintainer decision): the name no longer resolves, and every leading or
trailing `::` is an `EmptyEdge`, as it already was when parsed (#18977).
