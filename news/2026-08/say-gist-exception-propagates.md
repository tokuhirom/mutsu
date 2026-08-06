# `say` propagates an exception raised while computing `.gist`

```raku
sub f() { gather { take 1; die "boom-after-take" } }
say f().list;        # raku: dies with "boom-after-take"
say "after f";       # mutsu used to print an empty line, then "after f", exit 0
```

`render_gist_value` (`src/runtime/io_env.rs`) fell back to the native gist
on *any* error from the `.gist` call. The fallback is meant for a dispatch
failure (no `.gist` candidate to call), but it also ate a genuine user
exception thrown from inside `.gist` — including one thrown while `.gist`
forces a lazy `Seq`, which is how a `die` inside a routine-created
`gather` vanished into an empty output line. (Found while fixing
`samewith` inside a lazy gather — see
`news/2026-08/samewith-inside-lazy-gather.md` — and it is what
`todo/tickets/digest-dist-blockers.md` §6 called "a second problem in how
the failing gather is sunk".)

The fallback is now narrowed to real dispatch failures via the existing
`RuntimeError::is_method_not_found()` predicate (which covers both shapes
mutsu uses — a typed `X::Method::NotFound` instance and plain
message-prefixed errors) plus `is_multi_no_match()` for a `.gist` multi
with no matching candidate. Every other error propagates. The dedicated
`return`-control re-wrap (integration/error-reporting.t test 21) is
unchanged.

`render_str_value` (`put`/`print`) still swallows errors — the pre-existing
`TODO` on it stands; narrowing it is a separate slice since its callers
expect an infallible `String`.

Pinned by `t/say-gist-exception-propagates.t` (4 cases, verified against
raku).
