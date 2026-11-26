# `.split` pieces share the invocant's buffer

A `Str` body can now be a slice: `StrBody::Slice` is a byte range of a flat
body and shares its buffer. MoarVM's substring strands do the same thing.
`.split` uses it for every piece cut at a byte offset, so a piece costs one
allocation (its `Str`), not a copy of its text plus that allocation (#19201).

- `StrBody` now derefs to `str` instead of `String`, so a slice can hand out a
  `&str` without materializing. Readers are unchanged, because `as_str()` is
  now an inherent method. The handful of sites that cloned the `String`
  through the deref now say `to_string()`.
- A slice's base is always a flat body. A slice of a slice re-bases onto it,
  and a piece of a strand list is copied, so reading a slice is one hop.
- So that one small piece cannot pin a huge string, a base over 64 KiB is
  shared only by a piece at least an eighth of its size. Smaller pieces of
  such a base are copied, as before.
- `.split` no longer copies the invocant to get a `&str`: it borrows the
  invocant's own body.

The same measurement turned up a cost every `for … -> $x` loop paid. It built
a `String` key for its parameter on every iteration only for `Env::insert` to
intern it. The loop now interns the name once.

Measured with callgrind (`MUTSU_JIT=off`) against `main` at `13320ba6`:

- a 20-piece `split(' ')` makes ~1.6 allocations per piece, down from ~4.6
  before #19202 and ~2.7 after it;
- `benchmarks/word-count.raku` makes 327,861 `malloc` calls, down from
  727,855 (-55%), and executes 744.2M instructions, down from 850.4M
  (-12.5%).
