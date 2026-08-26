# `<?ww>` / `<!ww>` implemented, unblocking the custom `ws` override idiom

A grammar that overrides `ws` with the documented `token ws { <!ww> \h* }` idiom
(`raku-doc/doc/Language/regexes.rakudoc:2623`, and the `IniFormat` example in
`regexes-best-practices.rakudoc:163`) crashed outright on mutsu with
`No such method 'ww' for invocant of type 'Match'`.

## Root cause

`<?ww>` and `<!ww>` — the "within word" zero-width assertions — were simply not
implemented. `<?wb>` / `<!wb>` were, so `<!ww>` fell past the boundary-assertion
arms of the regex parser into `is_subrule_lookahead_name`, which compiled it as a
negative lookahead of a *user subrule* named `ww`. At match time that dispatched
`.ww` as a method on the cursor and blew up.

The custom-`ws` half of the ticket turned out to be a red herring: a plain
`token ws { \h* }` override was already consulted correctly by `rule`'s implicit
inter-atom whitespace, matching raku on all four of the ticket's cases. Only the
`<!ww>` inside the override was broken.

## Fix

A new `RegexAtom::WithinWord { negated }`, parsed from `<?ww>` / `<?.ww>` /
`<!ww>` / `<!.ww>` and from the bare `<ww>` runtime-lookup fallback, alongside the
existing `WordBoundary`. It asserts that the position has a word character on
**both** sides.

Note that this is deliberately *not* the negation of `<?wb>`: a position outside
the string counts as non-word for either test, so `<?ww>` and `<?wb>` are both
false in the middle of a run of non-word characters. Verified against raku across
string start/end, between two word chars, and between a word char and a space;
`roast/S05-mass/rx.t` encodes the same twelve cases.

Pin: `t/grammar-dynvar-failgoal-ws.t`.
