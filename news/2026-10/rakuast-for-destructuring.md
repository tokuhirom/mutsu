# RakuAST: a `for` loop's destructuring parameters

After anonymous destructuring parameters in signatures, a `for` loop's own
destructuring parameters were the largest part of that refusal: 30 `t/`
files. Examples are `for %h.pairs -> (:$key, :$value)` and
`for @rows -> [$id, $name]`.

Rakudo 2026.09 renders them exactly like a signature's anonymous ones: a
`Parameter` with no target that holds the `sub-signature`, with
`is-array => True` for the bracket form. The pointy block has no implicit
`Any` type.

mutsu's parser named a lone pattern `__for_unpack` whatever its brackets,
and named one of several `__for_unpack_N`, so the form was lost. A bracket
pattern is now `__for_unpack_array`, or `__for_unpack_array_N` among
several. Only the parser and RakuAST read these names; the compiler declares
the parameter under the name it carries and unpacks it from there. The
converter recognises both families. The lowering gives the loop's
parameters the same names back, numbered by position when there are several.

The `.AST` text of `for @x -> [$a, $b]`, `-> ($c), [$d]` and
`-> Pair (:$key)` now matches rakudo's.
