# `X::Syntax::Signature::InvocantNotAllowed` and `X::Syntax::NoSelf` now compose `X::Comp`

More residue from the `todo/tickets/vendor-real-test-module.md` campaign's
"`Got: X::Syntax::Confused`" cluster: `sub foo($a:) { }` (an invocant marker
in a *sub*, not a method) and `sub bar($!x) { }` (an attribute-twigil
parameter, which needs `self`) already raised the correctly-named exception
class — `reject_invocant_in_sub`/`reject_attr_params_in_sub`
(`src/parser/stmt/sub/traits.rs`) built the right `Value::make_instance` —
but `roast/S06-signature/errors.t` still saw `X::Syntax::Confused`.

Two bugs, the same shape as the preceding `X::Syntax::Comment::Embedded` fix:

1. **Neither class was registered under `X::Syntax` in `runtime_init.rs`.**
   `throws-like X::Syntax::Signature::InvocantNotAllowed` matches by class,
   but roast's neighboring `~~ X::Comp`-style checks (and the class's own MRO)
   never resolved without a `register_x` entry. Registered
   `X::Syntax::Signature::InvocantNotAllowed` under `X::Syntax::Signature` and
   `X::Syntax::NoSelf` under `X::Syntax` directly, matching
   `old-design-docs/S32-setting-library/Exception.pod`.
2. **Both `.message` attributes leaked the `"X::Type: "` message-convention
   prefix verbatim** instead of storing just the description text — a caller
   reading `$!.message` (rather than `.^name`) got the redundant class name
   glued to the front. Fixed by storing the bare description in the
   exception's `message` attribute while keeping the prefixed form only in
   the `PError` used for outer classification. Also updated
   `InvocantNotAllowed`'s wording to rakudo's actual message ("Can only use
   the : invocant marker in the signature for a method" — it previously said
   something rakudo doesn't).

`roast/S06-signature/errors.t` goes from 4 to 2 remaining failures under
`MUTSU_REAL_TEST=1` (the other 2, `-> $a: { }` / `-> $a: $b { }`, are a
separate, unfixed gap: pointy-block signatures don't even parse the `:`
invocant marker, so `reject_invocant_in_sub` — only wired into `sub`
declarations — never runs for them). Pinned by
`t/invocant-marker-exception-classes.t`, verified byte-identical to `raku`.
