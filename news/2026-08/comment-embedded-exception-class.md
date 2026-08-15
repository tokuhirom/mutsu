# `#\`` without an immediate opening bracket now raises `X::Syntax::Comment::Embedded`

Continuing the `todo/tickets/vendor-real-test-module.md` campaign's residue of
"`Got: X::Syntax::Confused`" files: `#\`` (the embedded-comment sigil) not
immediately followed by an opening bracket used to fall through to the
generic "Confused." diagnosis instead of the specific
`X::Syntax::Comment::Embedded` rakudo raises, even though the parser had
already produced the exact right message ("Opening bracket required for #\`
comment") — it just was not spelled in the `"X::Type: text"` convention that
`RuntimeError::split_typed_message_convention` looks for, so it stayed
untyped.

Two independent gaps had to be fixed, not one:

1. **The message itself wasn't typed.** `src/parser/helpers.rs`'s `ws()`
   raised a plain `PError::expected(...)` for this case; switched to
   `PError::fatal_at` (this diagnosis is never something another alternative
   could still match, exactly like its sibling "Couldn't find terminator for
   #\` comment" a few lines above) with the message prefixed
   `X::Syntax::Comment::Embedded: `.
2. **The class wasn't registered at all.** Even after typing the message,
   `X::Syntax::Comment::Embedded` was missing from `runtime_init.rs`'s
   `register_x` calls entirely, so `$! ~~ X::Comp` — the check
   `roast/S02-lexical-conventions/comments.t` uses for the "no space
   allowed"/"no tab allowed" variants it can't yet classify more precisely —
   failed even once the class name was right. Registered under `X::Syntax`
   (which already does `X::Comp`), matching `old-design-docs/S32-setting-library/Exception.pod`'s
   `X::Syntax::Comment::Embedded does X::Syntax`.

`roast/S32-exceptions/misc2.t` and `roast/S02-lexical-conventions/comments.t`
both stay fully green under the native `Test` provider (no behavior change on
the paths those exercise) and each lose one failure under
`MUTSU_REAL_TEST=1` (comments.t: 4 → 1 remaining, the unrelated unspace-in-
comment "sanity check"; misc2.t: 7 → 6). Pinned by
`t/comment-embedded-exception-class.t`, verified byte-identical to `raku`.
