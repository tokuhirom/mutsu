# URI::Encode percent-decoding now works through named substitution subs

`URI::Encode` 1.0 uses a named sub as the replacement callback for
`.subst`: `uri_decode` passes `&dec` to turn each run of `%XX` captures into
bytes and decode them as UTF-8. mutsu treated that declared callable as an
anonymous closure, evaluated its empty closure body, and replaced every
percent-encoded run with an empty string. The distribution's two baseline
files therefore passed encoding but lost seven assertions in `t/Encode.rakutest`
and six in `t/UTF-8.rakutest`.

Substitution replacement dispatch now sends declared subs (and Routine values)
through the normal callable binder with the current `Match`, while preserving
the existing environment-injection path for anonymous replacement blocks.
This is general `.subst` behavior, not a distribution-specific workaround.

Pinned by `t/regex/subst/subst-named-sub-replacement.t`. `URI::Encode` moves
from red (0/2 parity files, 22/35 assertions) to green (2/2, 35/35).

This run was locked on [#8977](https://github.com/tokuhirom/mutsu/issues/8977).
