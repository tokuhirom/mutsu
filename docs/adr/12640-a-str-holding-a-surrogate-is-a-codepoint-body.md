# ADR-12640: A `Str` holding a surrogate is a codepoint body

- **Status**: Proposed (2026-10-20)
- **Deciders**: tokuhirom, Claude
- **Context**: [#12640](https://github.com/tokuhirom/mutsu/issues/12640); builds on the `Str`
  payload type of [ADR-0120](0120-str-payload-may-be-a-lazily-flattened-strand-list.md) and the
  shared string primitives of [ADR-0117](0117-str-methods-and-nqp-ops-share-one-routine.md).

## 1. Context

A Raku `Str` is a sequence of graphemes over *codepoints*, and a codepoint may be a UTF-16
surrogate (U+D800..U+DFFF). Rakudo keeps one in a `Str` and only fails when the string has to
become UTF-8:

```
$ raku -e 'my $s = "a\x[D800]b"; say $s.chars; say $s.ords; say $s.substr(1,1).ord;
           say ($s ~ "c").chars; say $s eq "a\x[D800]b"; say $s.uc.ords;
           say $s.encode("utf16").list; say $s.encode("utf-8", :replacement).list;
           $s.encode'
3
(97 55296 98)
55296
4
True
(65 55296 66)
(97 55296 98)
(97 239 191 189 98)
Error encoding UTF-8 string: could not encode Unicode Surrogate codepoint 55296 (0xD800)
```

`say $s` fails the same way: printing is an encoding boundary.

A mutsu `Str` payload is an `Arc<StrBody>` that derefs to a Rust `String` (ADR-0120). A Rust
`String` is valid UTF-8 by type, and UTF-8 cannot represent a surrogate, so there is no byte
sequence for `"\x[D800]"`. Today the escape decoder rejects it (`X::Backslash::...`), `chr(0xD800)`
drops it, and a `Uni`/`ords` round trip loses it. Building a `String` with WTF-8 bytes in it is
undefined behaviour and is excluded by the issue and by docs/security.md (no unsafe string
construction).

There is also no free in-band space: every Unicode scalar value, private-use and noncharacter
codepoints included, is a legal `Str` codepoint, so mapping surrogates onto some reserved scalar
range would make a genuine character in that range indistinguishable from a surrogate.

## 2. Decision

1. **`StrBody` gains a third form, `Wide`**, used only for a string that contains at least one
   surrogate:

   ```rust
   pub enum StrBody {
       Flat(String),
       Lazy(Box<LazyStr>),
       Wide(Box<WideStr>),
   }
   struct WideStr {
       cps: Vec<u32>,            // the exact codepoints, NFC-normalized around non-surrogates
       lossy: OnceLock<String>,  // the `&str` view, built on first deref
   }
   ```

   A string with no surrogate is never `Wide`: every producer that would build a `Wide` body
   checks, and builds `Flat` instead when none is left (`.substr` of the scalar part, `.subst`
   removing it). So the existing fast paths, and every `Str` written by programs that never meet
   a surrogate, are untouched.

2. **The `&str` view of a `Wide` body is lossy**: `Deref<Target = String>` yields the codepoints
   with every surrogate replaced by U+FFFD. A reader that has not been taught about `Wide` sees a
   well-formed string of the right length in codepoints, never undefined behaviour and never a
   crash. This is the same contract as Rust's `String::from_utf16_lossy`.

3. **The primitives that define a `Str`'s identity read the codepoints, not the view.** They live
   in `src/builtins/str_prim/` (ADR-0117), so each gains one `Wide` arm, in one place:
   construction (`chr`, `\x[...]`/`\c[...]` escapes, `Uni.Str`, `nqp::chr`, `chrs`), `.ord`,
   `.ords`, `.chars`/`.codes`, `.substr`/indexing, `eq`/`cmp`/`eqv`/`ne`, hashing for hash keys,
   `~`/`x`/`join` (a `Wide` operand makes a `Wide` result), `.uc`/`.lc`/`.fc`/`.tc` (a surrogate
   maps to itself), `.flip`, `.index`/`.contains`/`.starts-with`/`.ends-with`, `.raku` (emits
   `\x[D800]`) and `.NFC`/`.NFD`/`.NFKC`/`.NFKD`/`Uni` (a surrogate is its own decomposition and
   blocks nothing).

4. **Every encoding boundary checks for `Wide`.** `.encode`, `print`/`say`/`put`/`note`, the
   `IO::Handle`/`IO::Socket` writers, `spurt`, `Proc` arguments and environment, `nqp::encode`,
   and NativeCall `Str` marshalling: UTF-8, UTF-8-C8, Latin-1, ASCII and Windows code pages
   raise Rakudo's `Error encoding UTF-8 string: could not encode Unicode Surrogate codepoint N
   (0xH)` (with the target encoding's name), or emit the encoding's replacement under
   `:replacement`; UTF-16 and UTF-32 encode the surrogate as its own code unit, as Rakudo does.
   The boundary must never fall back to the lossy view silently: a lossy U+FFFD reaching a file
   is the wrong answer this ADR exists to prevent.

5. **Anything else reached through the lossy view is a known gap, not a contract.** A regex
   match, `.split`, `.trans`, `sprintf`, a `Str` method not in item 3 — each sees U+FFFD for
   the surrogate, and a result built from that view loses it. Each such gap is a `todo:ticket`
   with a `raku` comparison, fixed by giving that primitive its `Wide` arm. None of them can
   corrupt memory or abort.

## 3. Consequences

- Construction, `.ord`, `.chars`, indexing, equality and the encoding failure match Rakudo,
  which is the whole scope of #12640, including `RakuAST` round trips (the AST node holds the
  `Str` value; `.raku`/`.DEPARSE` print the escape).
- One more predictable branch on the deref path (a third tag, never taken by ordinary strings)
  and a `Wide` arm in each identity primitive of item 3; no change to `Flat`/`Lazy` callers.
- A `Wide` body costs 4 bytes per codepoint plus its lossy view once read. Surrogates are rare
  (test suites, UTF-16 tooling, `Uni` manipulation), so this is not a size concern.
- The lossy view makes the remaining gaps visible as U+FFFD in output rather than as a crash,
  and item 4 guarantees no such U+FFFD is written out silently.

## 4. Rejected alternatives

- **WTF-8 bytes inside the `String`.** One buffer for every string and no new variant, but it
  breaks `String`'s UTF-8 invariant: every `&str` reader (~2,800 sites) would be undefined
  behaviour on such a string. Excluded outright.
- **Mapping surrogates onto a private-use or noncharacter range, with a per-string flag.**
  Keeps a `String` buffer, but a string carrying the flag that is concatenated with one holding a
  genuine codepoint in that range cannot represent both; and every producer that builds a new
  `String` from a `&str` would have to propagate the flag or silently turn the surrogate into a
  private-use character. A wrong answer that looks right is worse than item 2's visible U+FFFD.
- **Making every `Str` a codepoint (or grapheme) array, as MoarVM does.** The faithful model,
  and it would also give O(1) indexing, but it rewrites every `&str` reader for a case almost no
  program meets. If mutsu later adopts NFG storage, `Wide` folds into it.
- **Keep rejecting the escape.** Honest, but leaves `chr(0xD800)`, `Uni`, `Buf.decode('utf16')`
  and `nqp::chr` wrong or lossy, which is what #12640 reports.

## 5. Open questions

- Whether a `Wide` body should keep NFC normalization across a surrogate boundary exactly as
  MoarVM does (a surrogate has no canonical combining class, so it is a normalization boundary;
  this ADR assumes so and the implementation checks it against `raku`).
- Which UTF-16 decoder inputs should produce a `Wide` string instead of an error (Rakudo's
  `Buf.decode('utf16')` behaviour on a lone surrogate is to be measured, not assumed).
