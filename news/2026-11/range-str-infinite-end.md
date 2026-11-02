# `Range.Str` prints `*` for an infinite end and never expands an unbounded Range

`(1..Inf).Str` was `1..9223372036854775807` and `(-Inf..5).Str` expanded into an
endless `-Inf -Inf ...`. An infinite or Whatever end now stringifies as `*`
(`1..*`, `*..5`, `1..^*`), as rakudo does, and a `-Inf`/`*` start keeps range
notation instead of iterating. `.chars` follows (#16072).
