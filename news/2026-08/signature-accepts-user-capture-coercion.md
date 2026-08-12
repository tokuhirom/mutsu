# Sub-signature destructuring now honors a class's user-defined `Capture` method

A class may override `Mu`'s default `.Capture` to control how it
destructures against a sub-signature parameter (e.g.
`Cro::HTTP::Body::MultiPartFormData does Associative`, whose `method
Capture()` builds the named-arg hash from its `@.parts` array rather than
reflecting its own public attributes directly). Both `Signature.ACCEPTS`
and real block binding used to destructure any class instance by reading
its raw attributes straight from the object, ignoring a user-defined
`Capture` override entirely — so a class like this never matched a
destructuring sub-signature such as `-> (:$name!, :$surname!) {...}`.

Fixed by coercing through the user-defined `Capture` method first (when the
class actually defines one — a plain class without an override keeps the
previous, correct default behavior of reflecting its public attributes) in
both `signature_accepts_value` (the `Signature.ACCEPTS`/`~~ Signature`
path) and `bind_sub_signature_from_value` / `sub_signature_matches_value`
(the real call-binding path). Pin: `t/signature-accepts-user-capture-coercion.t`.

## Discovery

Found while re-measuring `t/http-router.rakutest` (vendored Cro::HTTP suite)
after the router-matching fixes in PR #6292: the router itself matched
correctly, but `request-body`'s own dispatch (`$handler.signature.ACCEPTS(\(body))`)
rejected a parsed `multipart/form-data` body against
`request-body -> (:$name!, :$surname!) {...}`, because
`Cro::HTTP::Body::MultiPartFormData`'s custom `Capture` method was never
consulted.

## Effect

- `t/http-router.rakutest` no longer dies mid-file on the multipart
  destructuring case — the file now runs to completion (`1..439`, up from
  dying after test ~360) for the first time, surfacing two further,
  unrelated pre-existing bugs now tracked separately:
  `todo/tickets/for-loop-var-shared-across-nested-closure-captures.md` and
  `todo/tickets/static-resource-content-type-mismatch-and-related-failures.md`.
