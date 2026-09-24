# Str coercions, comparisons and limited splits stop copying the whole string

Issue [#9147](https://github.com/tokuhirom/mutsu/issues/9147) collected the `Str`
operations whose *per-call* cost was worse than Rakudo's: none of them changed a
loop's order, but each call paid for the whole invocant when the answer needed
only a constant or a prefix of it. Most of them had the same root cause — a
`to_string_value()` / `to_str_context()` that copies the `Arc<String>` payload
into a fresh `String` just to read it — and the same fix: borrow the payload
(`Value::as_str`) and, where a value is returned, clone the `Value` (a refcount
bump) instead of rebuilding it.

## What changed

- **Identity coercions share the invocant.** `Str.Str` / `.Stringy`, prefix `~`
  (`StrCoerce`) and a single-part interpolation `"$s"` (`StringConcat(1)`) return
  the Str they were given. A user `prefix:<~>` candidate is still consulted first.
- **`Str.WHICH` is O(1).** The `ValueObjAt` now keeps the invocant itself under a
  private attribute and renders `Str|<text>` only when something reads it
  (`AttrMap::objat_which`, used by every ObjAt reader: `.Str`/`.gist`/`.raku`,
  `===`, `.WHERE`). `"abc".WHICH === ValueObjAt.new("Str|abc")` still holds.
- **`chomp` with nothing to chomp** returns the invocant (`builtins::chomp_value`,
  shared by the method and the sub).
- **String comparisons borrow.** `eq`/`ne` (O(1) on different lengths),
  `lt`/`gt`/`le`/`ge`, `leg`, `cmp`, `before`/`after`, `min`/`max` and the
  reduction forms compare `Cow<str>` views (`Value::str_context_cow`,
  `Value::string_value_cow`, and `stringify_compare_operand` now returning a
  `Cow`), so they cost O(common prefix) as in Rakudo.
- **`Uni.codes` / `.elems` / numeric** read the codepoint array's length instead
  of rebuilding the text and counting it.
- **A limit stops the scan.** `lines($limit)` (`split_lines_limited`),
  `words($limit)` (lazy `split_whitespace().take(n)`), `comb(Int, $limit)` and
  `comb(Str, $limit)` stop after the k-th piece; `comb(Regex, $limit)` stops the
  regex search after the k-th match (`regex_find_all_limited`,
  `regex_find_all_with_caps_limited`) and extracts the matched substrings with
  one forward walk instead of collecting every char of the subject.

## Measured

Release build, 20 000 calls on a 1M- and a 2M-char string, seconds. "Before"
is the figure quoted in the issue, except for `"$s"`, which was measured on this
branch before its fix went in:

| operation | after, 1M | after, 2M | before, 1M | before, 2M |
| --- | ---: | ---: | ---: | ---: |
| `$s.Str` | 0.021 | 0.020 | 1.15 | |
| `$s.WHICH` | 0.039 | 0.040 | 1.25 | |
| `"$s"` | | 0.0035 | 3.84 | 7.86 |
| `$s eq $o` (different lengths) | 0.018 | 0.010 | | |
| `.lines(3)` x20 (200k / 400k lines) | 0.0001 | 0.0001 | 0.40 | 0.46 |

`scripts/str-complexity-check.sh` now reports ratio ~2 (O(1) per call) for
`Str.Str`, `Str.WHICH`, `eq (different lengths)` and `chomp (nothing to chomp)`.

## Still open on #9147

- `x` builds its result eagerly (O(n * c)); Rakudo returns one repeat strand in
  O(1). The full-result NFC pass the issue also named was already gone (#9141).
- A named argument / containerized Pair copies its Str key (`MakeNamedArg`,
  `ContainerizePair`): `Value::Pair` stores the key as an owned `String`.
- `.comb.head(3)` needs a lazy `.comb` Seq.
- `comb(Regex, $limit)` still collects the subject's chars (O(n)) before the
  limited search starts.

Pinned by `t/types/string/str-per-call-bounds.t`.
