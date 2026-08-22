# `m:st(...)` regex adverb (starting positions) is unsupported

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
2684).

## Repro

```
my $data = "f fo foo fooo foooo fooooo foooooo";
say $data ~~ m:st(1|8)/fo+/;
```

- raku: `True`
- mutsu: `Runtime error: Unsupported regex adverb :st`

`:st(positions)` restricts matching to only start at one of the given position(s) (a junction or
list of integer offsets).

## Root cause

Simply unimplemented — the regex-adverb table in `src/runtime/regex.rs` /
`src/runtime/regex_parse.rs` doesn't have a `:st` entry (unlike `:pos`/`:continue`, which were
already fixed per `docs/doc-diff-backlog.md`'s Resolved section — #4996).

## Suggested next step

Read the `:pos(N)`/`:continue(N)` implementation (already correct per #4996) as a model — `:st`
is a related-but-distinct adverb that restricts the match's *starting* position(s) to a
set/junction of offsets rather than resuming from a single position.
