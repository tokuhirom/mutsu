# A backreference inside a group now sees the enclosing pattern's captures

`/ $<x>=(\w) [ $<x> ] /` did not match `"aa"` under mutsu, though it does under
raku. Neither did the positional twin `/ (\w) [ $0 ] /`, nor a backreference in
an alternation branch, a conjunction, a `~` goal, or two groups deep. Only the
completely flat form — no brackets at all — worked.

## Root cause

mutsu's regex engine matches every inline sub-pattern (a `[...]` group, an
alternation branch, a lookaround body) with its **own** capture store: the walk
builds a fresh `CapStore` seeded from `RegexCaptures::default()` and merges the
resulting delta back into the parent on success (ADR-0007). That is what keeps
per-step capture cost O(delta). But both backreference arms in
`regex_match_capture.rs` resolve against `current_caps` — the store of the level
they are executing in — so inside a group they were looking at an empty store
and always failed.

## What changed

A nested walk's base store now carries a read-through link to the enclosing
level's captures (`RegexCaptures::outer_backref`, an
`Option<Arc<OuterBackrefCaps>>` holding that level's named + positional slots and
a link to *its* parent). The two backreference arms consult the chain when the
name/index is absent locally. The link is never merged, never published outward,
and is cleared off every match the walk returns, so it cannot leak into a
capture tree.

Which sub-patterns get the link is pinned against real raku, because Raku does
not share the capture scope everywhere:

| Construct | Sees enclosing captures? |
| --- | --- |
| `[ ... ]` non-capturing group | yes |
| `|` and `||` alternation branches | yes |
| `&&` conjunction | yes |
| `~` goal | yes |
| `( ... )` **capturing** group | **no** — rakudo gives it its own cursor |
| `<?before ...>` / `<?after ...>` lookaround | **no** — same |
| a subrule call (`<name>`) | **no** — a different regex entirely |

A barrier is not merely "no link": it also hides every outer level from anything
nested deeper inside it, so `/ $<x>=(\w) ( [ $<x> ] ) /` fails under mutsu
exactly as it does under raku.

The mechanism mirrors the existing `INLINE_REGEX_VARS_SEED` (which does the same
job for a regex's `:my`/`:let` lexicals): a scoped thread-local seed armed for
the duration of one atom match. Cost is kept off the common path twice over — a
process-global flag set the first time the parser lowers *any* backreference
atom short-circuits the whole mechanism, and beyond that only a sub-pattern that
actually contains a backreference pays for a snapshot.

## Why it mattered

Found while re-measuring the `XML` battery candidate
(`docs/batteries/xml.md`). `XML::Grammar`'s `element` token closes with

```raku
[ '/>' | '>' <child>* '</' $<name> '>' ]
```

so under mutsu no XML element with a closing tag matched at all — `<root/>`
parsed, `<root></root>` did not. Fixing it took `XML` from 2/15 to 5/15 upstream
test files.

Pinned by `t/regex-backref-in-group.t`, which asserts all seven sharing
constructs and all four barrier cases, each verified to agree with `raku`.
