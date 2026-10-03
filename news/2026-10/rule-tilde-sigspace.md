# `rule` sigspace around the `~` goal construct matches Rakudo

In a `rule`, `'[' ~ ']' <k>` used to wrap the inner atom in whitespace on both
sides whenever a space was written before the `~`, and placed the whitespace
written after the inner atom *after* the closer. Rakudo lays it out differently:
the space before `~` allows whitespace after the opener only, the space after
the inner atom allows whitespace before the closer (inside the construct), and
the space after the goal atom allows whitespace after the closer.
`rewrite_tilde_tokens` now follows that layout, so `rule {'['~']'<k> 'z'}`
accepts `[a ]z` and rejects `[a] z`, as Rakudo does (#11234).
