# `.subst` / `s///` replace the `<( … )>` region, not everything the pattern consumed

```raku
say 'xaby'.subst(/a <( b )>/, 'Z');            # raku: xaZy   was: xZy
say 'a-w'.subst(/. <( '-' )> ./, '..', :g);    # raku: a..w   was: ..
```

`<(` and `)>` narrow a match to the marked region even though the pattern
consumed more — that is the whole point of them, and it is how you write "match
`-` but only when it has a character on each side". `.subst` spans its
replacement with the match, so ignoring the markers overwrote the context the
pattern had only *looked* at.

## Root cause

Smart-matching was already correct: `'xaby' ~~ /a <( b )>/` reports `.from` 2,
`.to` 3, and every general match entry point (`regex_match_with_captures_core`,
`regex_match_all_with_captures`, the `.parse` finder) narrows `caps.from`/`.to`
to `capture_start`/`capture_end` when the markers are present.

`regex_find_first_from_with_captures` did not. It returned the raw
`(start, end)` of the consumed span, and it is what `.subst`'s native fast path
(`vm_native_subst.rs`) walks to place each replacement — so exactly the marked
case came out wrong, in both the `.subst` method and the `s///` operator, global
and not. It now narrows the same way its siblings do, in both its plain and its
`:m`-ignoremark branch.

## Impact

`File::Ignore` (`TODO_dist` T-050) compiles a `[a-w]` character range into a
Raku character class with `$/.subst(/. <( '-' )> ./, '..', :g)` — turning `a-w`
into `a..w`. Under the old behaviour that produced `..`, so every range-bearing
ignore rule compiled to a broken pattern. `t/range.rakutest` now passes, and with
it six of the dist's seven files are clean (only `t/walk.rakutest` remains — a
separate nested-`sub`-in-`gather` bug, recorded as
`todo/tickets/nested-sub-in-gather-under-doubly-nested-class.md`).

Pin: `t/subst-capture-markers.t` (8 assertions, each verified against raku:
both-sided, leading-only and trailing-only markers, `:g`, the `s///` operator
form, and a marker-free pattern as the control).
