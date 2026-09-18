# A non-ASCII `$s ~= ...` is linear too

[#8695](https://github.com/tokuhirom/mutsu/issues/8695) made `$s ~= 'x'` linear
by fusing the statement into `OpCode::ConcatAssignLocal` and appending into the
accumulated buffer in place — but only for an **ASCII** suffix. Every other
suffix fell through to `concat_values`, which rebuilds the whole string and
re-runs NFC over all of it, so it stayed O(len($s)) per append with a *larger*
constant than the original ASCII case ever had. At 20,000 appends of
`"\c[SNOWMAN]"` that was 318x rakudo ([#8725](https://github.com/tokuhirom/mutsu/issues/8725)).

The ASCII restriction was not arbitrary. Growing the buffer is only sound if
the result is still NFC, and the append may skip normalization only when the
join cannot compose. An ASCII character is a starter and never a combining
mark, so `lhs ~ "x"` is NFC whenever `lhs` is. Nothing that cheap holds for a
general suffix: `"e" ~ "\x[301]"` is a single `é`, and "the suffix starts with
a starter" is not the test either, because Hangul composes starter with
starter (`\x[1100]` + `\x[1161]` is one syllable).

## What the append actually needs to know

NFC is *local*. Normalization only composes or reorders characters within one
normalization segment, and a segment can never begin before a character that
has a **boundary before** it: combining class 0 *and* `NFC_QC = Yes`. Both
halves matter — a mark that never composes still has a non-zero class and can
be canonically reordered, and a Hangul V/T jamo is a class-0 starter that
`NFC_QC` reports as `Maybe` precisely because it composes backwards.

That gives two shapes, decided from the suffix alone in O(len(suffix)) by
`StrAppendPlan::for_suffix` (`src/value/value_str_append_nfc.rs`):

- **Direct** — the suffix begins at a boundary, so the join cannot compose and
  the accumulated buffer is never read. This covers every ASCII suffix (the
  case #8695 already had) and the overwhelmingly common non-ASCII ones: a
  snowman, a CJK ideograph, an emoji, a composed `é`. The suffix itself is
  normalized if it is not already NFC, which costs the suffix's length, not
  the buffer's.
- **Join** — the suffix begins with a combining mark or a Hangul V/T jamo.
  Only a bounded window around the join is redone: `nfc_join_window` backs up
  over the buffer's trailing combining run to the last boundary character,
  that window plus the suffix is normalized as one piece, and the result is
  spliced back. A run longer than 64 characters has no interior boundary to
  find — `$s ~= "\x[301]"` in a loop builds one grapheme with n marks — so
  that case renormalizes the whole buffer, which is the cost it would have
  had anyway rather than that cost plus a full backward scan.

The `Value` representation is unchanged. #8725 sketched carrying a
normalization flag on the string value so the append could answer "is the
accumulated buffer NFC?" in O(1); that turned out not to be needed, because
the buffer being NFC is the same repo-wide invariant the ASCII path already
relied on, and the only genuinely new question — "can this particular join
compose?" — is answerable from the suffix.

## Result

Release build, 20,000 appends, timing the loop only (the "before" column is
#8725's own measurement):

| suffix | before | after | rakudo |
| --- | --- | --- | --- |
| `'x'` (ASCII) | 0.0066s | 0.0072s | 0.0135s |
| `"\c[SNOWMAN]"` | 5.98s | 0.0037s | 0.0182s |
| `"\c[CJK UNIFIED IDEOGRAPH-4E00]"` | — | 0.0070s | 0.0131s |

The ASCII path is untouched — its `is_ascii()` check still runs first, ahead of
any table lookup — and the non-ASCII case goes from 318x rakudo to about 5x
faster than it.

The one case that stays slow is the pathological one the window limit exists
for: `$s ~= "\x[301]"` 20,000 times builds a *single* grapheme with 20,000
combining marks and so has no interior normalization boundary anywhere. mutsu
takes 2.5s over it, essentially what the pre-#8695 general path cost, because
the window search gives up after 64 characters and renormalizes the whole
buffer. There is no faster answer that is also correct, and rakudo does not
answer it at all — it dies with "Too many codepoints (1024) in grapheme".

## Coverage

The window rule fails *silently* when it is wrong — a mis-sized window changes
`.chars` and string comparison rather than raising anything — so it is pinned
from both sides. `t/types/string/concat-assign-local-in-place.t` grew fifteen
assertions covering composition at the join, canonical reordering across it,
Hangul L+V and LV+T, an `NFC_QC = No` singleton, a non-NFC suffix, a combining
run past the window limit, and a long non-ASCII accumulation; every one of them
was checked against rakudo first. In Rust, `value_str_append_nfc`'s tests
include a differential check that builds 4,000 sequences from a corpus of
interacting characters (starters, marks of three combining classes, jamo, a
singleton, a composed letter), splits each at a random point, and asserts the
append matches normalizing the whole concatenation.
