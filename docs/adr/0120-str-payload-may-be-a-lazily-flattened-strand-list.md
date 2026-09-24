# ADR-0120: A `Str` payload may be a lazily flattened strand list

- **Status**: Proposed (2026-09-24; user decision to take the strand representation first and
  the neighbouring string issues on top of it).
- **Deciders**: tokuhirom, Claude
- **Context**: [#9253](https://github.com/tokuhirom/mutsu/issues/9253) (`x` builds the repeated
  string eagerly), [#9209](https://github.com/tokuhirom/mutsu/issues/9209) (infix `~` copies a
  shared left operand; interpolation copies every part), and the neighbours that build on the
  same payload type: [#9161](https://github.com/tokuhirom/mutsu/issues/9161) (`[~]` is
  quadratic) and [#9252](https://github.com/tokuhirom/mutsu/issues/9252) (a Pair / hash key
  copies the key string).

## 1. Context

A mutsu `Str` value is a NaN-boxed `Arc<String>`: one flat, NFC, UTF-8 buffer. Every consumer
reads it as `&str` (`ValueView::Str` is matched at ~860 sites, `as_str()` at ~1,900). That makes
reads cheap, but every *producer* pays for the whole result:

| producer | mutsu | Rakudo (MoarVM) |
|---|---|---|
| `$s x $c` / `nqp::x` | O(n*c) bytes written | O(1): one strand with a repeat count |
| `$a ~ $b`, `$a` still live | O(n1 + n2): the left buffer is shared, so it is copied | O(1): two strands |
| `"$a-$b"` (interpolation) | O(total chars) | O(parts): one strand per part |

MoarVM's `MVMString` is either a flat grapheme blob or a list of **strands**, each strand being
a reference to a flat string, a range in it, and a repetition count. Concatenation copies strand
lists (bounded by `MVM_STRING_MAX_STRANDS` = 64) instead of characters; when the list would grow
past that, the result is flattened. Reading the characters of a strand string either walks the
strands directly or flattens it.

mutsu's in-place append (#8695, #9141) already made the *unshared* accumulation `$s ~= $x`
amortized O(m). What is left are the producers whose operand is shared (so it cannot be grown in
place) or whose result is a multiple of its input (`x`).

## 2. Decision

1. **The `Str` payload becomes `Arc<StrBody>`**, where

   ```rust
   pub enum StrBody {
       Flat(String),
       Lazy(Box<LazyStr>),   // strands + a flatten-once cache
   }
   struct LazyStr { strands: Vec<Strand>, len: usize, flat: OnceLock<String> }
   struct Strand { base: Arc<StrBody>, reps: usize }
   ```

   `StrBody: Deref<Target = String>`. A `Flat` body derefs to its buffer; a `Lazy` body flattens
   **once** into its `OnceLock` on the first read and derefs to that from then on. Every existing
   reader therefore keeps reading a `&String` / `&str` unchanged; only code that needs the
   `Arc<String>` *type* changes.

2. **A strand's base is always a `Flat` body.** Building a strand list from a `Lazy` operand
   splices that operand's strands (cloning their `Arc`s, never its characters). Nesting depth is
   therefore one: dropping or flattening a body never recurses, and no chain of intermediate
   results is kept alive.

3. **At most `MAX_STRANDS` (64, MoarVM's number) strands.** A concatenation that would exceed it
   produces a flat result instead, exactly as MoarVM does. `~` is therefore O(1) amortized over 64
   concatenations of a shared operand, and interpolation is O(parts) up to 64 parts.

4. **Small results stay flat.** A result shorter than `STRAND_MIN_BYTES` (1 KiB) is built flat: a
   copy that small is cheaper than the flatten that the first read of a strand list pays, and it
   keeps short temporaries on the fast path every reader already has.

5. **Laziness never changes a result.** A strand join is only taken when the join cannot compose
   under NFC: the right-hand part begins at a normalization boundary
   (`has_nfc_boundary_before`); every `Str` is already NFC, so the concatenation is too. `x`
   strands only when its source "repeats as NFC" (the check #9141 introduced). Any other join
   takes the existing eager path, which renormalizes the join window.

6. **Unique writers flatten first.** `str_appended_nfc` (the in-place `~=` path) on a unique
   `Lazy` body turns it into `Flat` (taking the cached buffer when there is one, flattening
   otherwise) and then appends in place, so an accumulation that starts from a strand result is
   still amortized O(m) per append after one O(n) flatten.

7. **`x` enforces Rakudo's deterministic size cap** instead of relying on a fallible
   reservation: a count above 4294967295 dies with "Repeat count (N) cannot be greater than max
   allowed number of graphemes 4294967295", and a result above 4294967295 graphemes with "Can't
   repeat string, required number of graphemes (g * n) greater than max allowed of 4294967295".
   A lazily built string cannot report an allocation failure at construction time, so the cap is
   what keeps `try { "x" x 1e15 }` catchable.

## 3. Consequences

- `x`, `~` of a shared operand, and interpolation become O(1) / O(parts) to *build*; the first
  *read* of a strand result pays the O(n) flatten once, as MoarVM's flattening readers do. A
  result that is never read (the #9253 benchmark, `my str $a = "a" x 2**32-1` in
  `A01-limits/misc.t`) never allocates its characters.
- A `Lazy` body that has been read holds both its strands and its flat copy until it is dropped
  (at most 2x the string's size). Writers (item 6) drop the strands.
- Every read of a `Str` now matches on the body's tag before reaching the buffer: one
  predictable branch on the `Flat` path (plus an acquire load on an already-flattened `Lazy`).
- `Arc<StrBody>` is the type the key-sharing work of #9252 should adopt for `Value::Pair` keys
  and hash keys, so that making a key from a strand string shares it too.

## 4. Rejected alternatives

- **A balanced rope (tree of concatenations).** O(log n) concat and index without ever
  flattening, but every reader would have to walk it, which means rewriting the ~2,800 sites that
  read a `&str` today, or flattening anyway. MoarVM's bounded flat strand list gets the
  asymptotics that matter (build cost) with a single flatten on read.
- **Readers that walk strands directly** (`.chars`, `eq`, `.index` over a strand list without
  flattening). Possible later, per primitive, inside `src/builtins/str_prim/` (ADR-0117); not
  needed for any current goal, and each one would be a second implementation of a primitive to
  keep in step with the flat one.
- **Strands only for `x`.** It would meet #9253 alone, but `~` and interpolation have the same
  shape (a result that is mostly a shared operand) and would need the same payload type anyway.
- **Substring strands** (`.substr` returning a range into its invocant). MoarVM does this; it is
  a natural extension (a `Strand` gains a byte range) but retains the whole invocant for a small
  substring, so it is left for a measured need.

## 5. Implementation status

- Payload switch, lazy `x` / `nqp::x`, strand `~` and strand interpolation: this ADR's PR.
- #9161 (`[~]` collects parts and joins once), #9209's closure-captured / `$!attr` `~=` shapes
  (in-place append through a cell) and #9252 (share `Arc<StrBody>` keys): follow-up PRs.
