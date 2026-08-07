# String positions are grapheme-based

A Raku string is a sequence of **graphemes**, so every position and length a
string method reports or accepts is a grapheme index. `.chars` already counted
graphemes (`str.graphemes(true).count()`), but `substr`, `index`, `rindex` and
`indices` counted **codepoints** — so the two scales silently disagreed on any
string holding a multi-codepoint grapheme, and a position computed with one was
wrong when fed to the other.

```raku
"AAA\r\n--bnd".index("--bnd")   # raku: 4          mutsu was: 5
"\r\nHello".substr(1)           # raku: "Hello"    mutsu was: "\nHello"
```

The second is worse than an off-by-one: it returns a **torn grapheme**, half a
CRLF.

`\r\n` is one grapheme and is everywhere in wire protocols, so this was
load-bearing rather than exotic. Cro's `multipart/form-data` parser walks a body
with exactly that pair of calls —

```raku
$payload .= substr($start + $dd-boundary.chars);
my $next-boundary = $payload.index($search);   # $search = "\r\n$dd-boundary"
```

— so it lost a character per boundary and rejected every multipart body with
`Unexpected text after multpart/form-data boundary`.

## Fix

`crate::builtins::string_pos` holds the three conversions the string methods
need — `grapheme_units`, `grapheme_offset` (byte offset from `str::find` →
grapheme offset) and `grapheme_len` — and `substr`, `index`, `rindex` and
`indices` now go through them, in both the interpreter dispatch and the native
fast paths.

ASCII text with no `\r\n` has exactly one grapheme per byte, so all three take a
fast path that skips segmentation entirely. That covers the overwhelming
majority of strings, which is why this is a correctness fix rather than a
throughput one.

## Result

`Cro::HTTP::BodyParser::MultiPartFormData` parses. In
`t/http-request-parser.rakutest` the passing assertion count goes from **293 to
323**. The raw *failure* count goes 9 → 18 at the same time, and that is the
point: the multipart body parse used to die, so all of its per-part assertions
were skipped. They now run, and twelve of them fail on a separate issue — a
part with no `Content-Type` header should default to `text/plain`, and mutsu
gives it no content-type at all. That is a real, newly-visible bug rather than a
regression.

Pinned by `t/string-positions-are-graphemes.t` (18 assertions, byte-identical
output under real `raku`) and three unit tests in `builtins::string_pos`.
