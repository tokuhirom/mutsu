# A `Proc::Async` output chunk holds back its final grapheme

A decoder reading a pipe cannot know that the last grapheme it just decoded is
finished: the next `read()` could begin with a combining mark that extends it
into a different grapheme. Rakudo's `Proc::Async` output Supply therefore never
hands out the trailing grapheme of a chunk — it holds it back until a later read
proves nothing extends it, and flushes it alone when the stream ends. mutsu
emitted each decoded read whole:

```raku
my $proc = Proc::Async.new('sh', '-c', 'printf "abc"; sleep 1; printf "def"');
my @chunks;
react {
    whenever $proc.stdout { @chunks.push($_) }
    whenever $proc.start { }
}
say @chunks.raku;
# raku (2026.06): ["ab", "cde", "f"]
# mutsu (was):    ["abc", "def"]
```

The concatenation was identical, so anything accumulating the output or reading
it through `.lines` agreed. Only code observing chunk boundaries saw it — and
the case the holdback exists for, a combining mark arriving in the *next* read,
came out as a split grapheme:

```raku
# printf "ae"; sleep 1; printf "\314\201z"
# was:  ("ae", "\x[301]z")   -- the acute accent orphaned from its base
# now:  ("a", "é", "z")      -- three graphemes, as rakudo has it
```

## Where it became a content difference

On a stream that goes bad the two implementations disagreed about *content*, not
just boundaries. Rakudo discards the whole pending decode when the stream ends in
an error rather than flushing the valid prefix first:

```raku
# printf "ok-"; sleep 1; printf "\377\377"
#   raku: got="ok"  quit="quit"   -- the held-back "-" dies with the stream
#   mutsu (was): got="ok-"
# printf "ok-"; printf "\377\377"     (both writes in one read)
#   raku: got=""                  -- the whole pending decode is discarded
#   mutsu (was): got="ok-"
```

## Fix

`feed_utf8_incremental` (`src/runtime/native_proc_async.rs`) now keeps the
trailing grapheme in a `held` buffer instead of emitting it, and flushes it only
at a clean end of stream. On a genuinely malformed byte it emits nothing at all
from that read — not even the valid prefix it had already decoded — and drops
what it was holding, which is what makes both error cases above agree with
rakudo.

This *subsumes* the narrower mechanism it replaces. The function already held
back a lone trailing `\r`, in case the next read began with `\n`, so mutsu's
stdout `\r\n` → `\n` rewrite would not see a pair split across two reads. `\r\n`
is a single grapheme under UAX #29, so the general holdback covers that case by
construction and `held_cr`/`flush_held_cr` are gone. stderr, which never
translated CRLF and so never held anything, now flushes its held grapheme too —
without that it would have lost the last character of every stream.

A new helper, `builtins::string_pos::last_grapheme_start`, names the split
point, next to the grapheme-indexing helpers that already exist there.

## A decoded chunk is NFC-normalized

Writing the combining-mark test surfaced a second defect on the same path: the
chunk was delivered as raw NFD bytes. A Raku `Str` is NFG, so text decoded from
bytes must compare equal to the same text written as a literal — which is exactly
what `news/2026-08/decoded-strings-are-nfc.md` established for `.decode`, and the
live Proc::Async decoder had been missed:

```raku
$chunk.encode('utf-8')   # was (101 204 129), now (195 169)
$chunk eq "\x[e9]"       # was False, now True
```

`.ords` and `.chars` normalize on read, which is what hid it: every diagnostic
printed the same code points while `eq` and `.encode` disagreed. Normalizing
chunk-at-a-time is sound only *because* of the holdback — no grapheme spans two
chunks, so normalizing each one gives the same answer as normalizing the whole
stream.

## Tests

`t/procasync-final-grapheme-holdback.t` pins all of it (9 cases: stdout, stderr,
the merged `.Supply`, the combining mark, the CRLF pair split across reads, and
both malformed-byte shapes), against the rakudo 2026.06 answers recorded in the
ticket.

Two existing local tests encoded the old whole-read chunking and were corrected,
not worked around. `t/procasync-stdout-incremental.t` asserted "EARLY and LATE
arrived as two separate chunks"; under the holdback that is three chunks, and
since the file is about *when* chunks arrive, it now asserts the assembled output
plus the timing gap. `t/proc-async-merged-supply-react.t` asserted that a merged
two-stream output's *lines* could be sorted back into shape — but with each
reader holding its own final grapheme, a line's trailing `\n` can be flushed
after the other stream's chunk lands, so the concatenation splits that line. That
assertion is order-dependent under correct semantics (it would be flaky in rakudo
too) and is now the order-free invariant it was reaching for: the multiset of
characters. Its `done`-ends-the-react case expects `"first"` rather than
`"first\n"`, because the newline is still held back when `done` fires.

Closes `todo/tickets/procasync-output-chunks-do-not-hold-back-final-grapheme.md`.
