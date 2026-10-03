# Startup and `$s ~= 'x'` loops got ~20% faster

`string-concat` (`my $s = ''; $s ~= 'x' for ^10000`) was the one benchmark
where mutsu trailed Raku++ (CI: 10.4 ms against 8.7 ms, ratio 1.20). The bench
history showed why: `bench-startup` alone was 8.1 ms against 5.8 ms, so
mutsu's loop was already the faster half. The gap was startup, and this change
attacks both halves.

## Startup

Measured with callgrind on `say 1`, startup went from **10.16M to 7.66M
instructions (-25%)**, and from ~950 to ~700 page faults per process:

- **The qualified-member index is built lazily.** `qualified_tail_index` was
  fed from `Symbol::intern_global` for every new symbol — ~1M instructions per
  process, mostly `str::find("::")` over ~950 built-in names — for a lookup
  only a package-stash read ever makes. It now folds the symbol table's
  append-only id sequence in on its first read, exactly as its sibling package
  index has done since #10228, so it is still a guaranteed superset.
- **The interner's per-thread memo keys on the leaked `&'static str`** instead
  of an owned copy, saving one allocation per distinct name.
- **The built-in `X::` registration stops copying names**: the MRO walk
  borrows from the class table, the composed-role loop moves its lists
  instead of cloning them three times, and the accessor sync interns class
  names straight from the map's keys.
- The change also linked the Linux `mutsu` binary non-PIE to skip the
  loader's ~49,000 start-up relocations. That gave up ASLR for the main
  executable and was reverted: the binary is PIE again, and the page-fault
  and timing figures below include the part it contributed.

## The `~=` loop

Per iteration of `$s ~= 'x'` inside `for ^N`, ~1,450 instructions became
~1,270:

- `ConcatAssignLocal` grows a uniquely held buffer **where it stands**
  (`Value::try_append_str_nfc_in_place`, through a new
  `NanBox::str_body_mut`) instead of moving the value out of its slot,
  decoding it, re-encoding it and storing it back. A shared buffer still takes
  the copying path, so `my $b = $a; $a ~= 'x'` leaves `$b` alone.
- The int-range `for` loop rebinds its topic with `Env::rebind_sym`, which
  overwrites the overlay's existing slot instead of running the general
  `insert_sym` (copy-on-write probe plus a hashing map insert) every
  iteration.

## Numbers

Local A/B on a 4-core container, 150 interleaved runs each, median wall clock:

| | `main` | this change |
| --- | ---: | ---: |
| `say 1` | 7.12 ms | 5.73 ms |
| `string-concat` | 9.73 ms | 7.82 ms |

Instruction counts for `string-concat`: 24.66M → 19.51M. The bench CI's
`string-concat` row against Raku++ is the number to watch.
