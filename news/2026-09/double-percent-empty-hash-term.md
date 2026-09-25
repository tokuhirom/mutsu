# `%%` parses as the empty-hash term

[#9319](https://github.com/tokuhirom/mutsu/issues/9319), split out of the
[#7988](https://github.com/tokuhirom/mutsu/issues/7988) parse-gap cluster.
`my %prov = $d.meta<provides> // %%;` (RakuPM's `RakuPM/Client/Query.rakumod`)
died with `===SORRY!=== Confused ... expected expression after multiplicative
operator`, so `RakuPM::Client` could not load.

In rakudo a sigil followed by another sigil keeps the second one as part of the
variable: `@@a` is `@a` in list context and a bare `@@` is an anonymous array.
`array_var` already knew this (`@@` gave `[]`), but `hash_var` did not. On
`%%` it consumed the first `%` as the bare anonymous hash and returned, leaving
the second `%` for the operator loop, which read it as infix modulo and then
found no right-hand operand. The same gap made `%%h` evaluate to an empty
anonymous hash followed by a discarded `%h` term (with a "Useless use of %h"
warning) instead of `%h` itself.

`hash_var` now recurses on a second `%`, exactly as `array_var` does on a
second `@`: a bare `%%` is the anonymous (empty) Hash, and `%%h` is `%h`. The
infix `%%` divisibility operator is untouched, since `hash_var` is reached only
in term position.

Pinned by `t/lang/double-sigil-hash-term.t`.
