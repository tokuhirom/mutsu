# A pointy block's `:` invocant marker now raises `X::Syntax::Signature::InvocantNotAllowed`

Continuing the `todo/tickets/vendor-real-test-module.md` campaign:
`roast/S06-signature/errors.t` expects `-> $a: { }` and `-> $a: $b { }` to
raise `X::Syntax::Signature::InvocantNotAllowed`, the same class a sub
signature with an invocant already raised (`sub foo($a:) { }`). The sub path
worked; the pointy-block path did not.

The cause: `parse_pointy_param` (the pointy-block per-parameter parser,
`src/parser/stmt/control/pointy_param.rs`) never recognizes a trailing `:`
invocant marker at all — every `ParamDef` it returns hardcodes
`is_invocant: false`. The arrow-lambda driver
(`src/parser/primary/misc/lambda.rs::arrow_lambda_inner`) was then left
holding a literal `: { }` / `: $b { }` with no branch for it, and
`parse_block_body`'s `parse_char(input, '{')` failed on the leading `:` —
surfacing as the generic "Confused." parse error instead of a typed exception.

Fixed by checking for the marker directly in `arrow_lambda_inner`, both right
after the first parameter and after each subsequent parameter in the
multi-param comma loop, and raising the same
`X::Syntax::Signature::InvocantNotAllowed` class the sub path uses (factored
into a shared `invocant_not_allowed_error(context)` helper in
`src/parser/stmt/sub/traits.rs`). A pointy block can never declare an
invocant — only a method can — so the check fires unconditionally, with no
need to distinguish further contexts. Legitimate colon uses inside a pointy
signature (`:$named`, `where` clauses, `::T` type captures) are unaffected:
they are all consumed inside `parse_pointy_param` itself before it returns, so
they never reach this new check.

`roast/S06-signature/errors.t` now passes under both the native and the real
`Test` module (`MUTSU_REAL_TEST=1`). Pin: extended the existing
`t/invocant-marker.t` (which already covered the sub-side cases) with the two
pointy-block assertions, both verified byte-identical to `raku`.
