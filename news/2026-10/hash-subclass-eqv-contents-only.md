# `eqv` on an `is Hash`/`is Array` subclass compares contents only

Rakudo's `eqv` for an Associative/Positional compares the type and the
contents; the extra attributes a subclass declares never take part. mutsu
compared every public attribute, so `is-deeply $stream.COERCE($stream),
$stream` failed on two streams with equal dictionaries. Subclass instances now
compare their hash/array storage only. PDF 0.6.17 `cos-coerce` passes.
