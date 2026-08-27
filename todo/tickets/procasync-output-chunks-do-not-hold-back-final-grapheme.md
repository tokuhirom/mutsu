# `Proc::Async` output chunks do not hold back their final grapheme

Rakudo's `Proc::Async` output Supply never emits the last grapheme of a chunk: it
holds it back until the next read proves nothing extends it (a combining mark in
the following bytes would make it a different grapheme under NFG), and flushes it
alone when the stream ends. mutsu emits each decoded read whole.

## Repro — clean stream, no error involved

```raku
my $proc = Proc::Async.new('sh', '-c', 'printf "abc"; sleep 1; printf "def"');
my @chunks;
react {
    whenever $proc.stdout { @chunks.push($_) }
    whenever $proc.start { }
}
say "chunks=", @chunks.raku;
```

- `raku` (2026.06): `chunks=["ab", "cde", "f"]`
- mutsu: `chunks=["abc", "def"]`

The concatenation is identical (`"abcdef"`), so anything that accumulates the
output, or reads it through `.lines`, agrees. Only code that observes chunk
boundaries sees the difference.

## Where it becomes a content difference: malformed UTF-8

Held-back text that a decode error then invalidates is never delivered at all, so
on a stream that goes bad the two implementations disagree about *content*, not
just boundaries:

```raku
my $proc = Proc::Async.new('sh', '-c', 'printf "ok-"; sleep 1; printf "\\377\\377"');
my ($got, $quit) = ('', '');
react {
    whenever $proc.stdout { $got ~= $_; QUIT { $quit = 'quit'; done } }
    whenever $proc.start { }
}
say "got=", $got.raku, " quit=", $quit.raku;
```

- `raku`: `got="ok" quit="quit"` — the held-back `-` dies with the stream.
- mutsu: `got="ok-" quit="quit"`

And with both writes landing in a single `read()`
(`printf "ok-"; printf "\377\377"`, no `sleep`), `raku` gives `got=""` — the
whole pending decode is discarded — while mutsu still gives `got="ok-"`. `QUIT`
fires and `LAST` correctly does not, in every variant and on both the per-stream
and merged Supplies; only the delivered text differs.

## Root cause

`feed_utf8_incremental` (`src/runtime/native_proc_async.rs`) emits every byte it
successfully decodes from the current `read()` immediately — including, on a
`Utf8Error`, the `e.valid_up_to()` prefix, which it sends *before* checking
`error_len()`. There is already a precedent for the holdback mechanism in the
same function: `held_cr` keeps a trailing lone `\r` back in case the next read
starts with `\n`. Generalising that from "one `\r`" to "the final grapheme"
is the shape of the fix, plus discarding whatever is still held when the stream
ends in an error rather than flushing it.

Cost to weigh before implementing: holding a grapheme back delays it by one
`read()`, and the single-read case above suggests Rakudo discards more than just
the held grapheme on error, so confirm the exact rule against Rakudo first rather
than assuming. Also check that the merged `.Supply` and the `bin`-mode path stay
consistent, and that `todo/tickets/procasync-stdout-is-not-incremental.md`'s
streaming guarantee is not weakened.

Not covered by `roast/S17-procasync/encoding.t` (whitelisted and passing), which
does not exercise valid-then-invalid output or chunk boundaries.

Found while fixing `news/2026-08/procasync-merged-supply-is-live-in-react.md`.
Pre-existing on the `.stdout` path and unchanged by that work; the merged Supply
inherits it because it shares the same decoder.
