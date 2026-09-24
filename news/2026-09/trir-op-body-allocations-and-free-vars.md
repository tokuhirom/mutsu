# TRIR stops allocating per call and resolving `$ws` per call (ADR-0116 D2.4)

ADR-0116 D2 shrinks the bodies of the ops TRIR runs, because native lowering
can only remove the switch loop's own share of a JSON::Fast record. This slice
does D2.4 (allocation). While re-measuring, it also found three per-call costs
the ADR's table did not name.

## Re-measured first

The 1-vs-101-record callgrind difference (ADR-0116 §8) was re-run on `main`
after D2.1-D2.3. This time it decoded a pre-generated JSON file, so building
the document no longer counts in the difference. That leaves **107.3 M
instructions and 43,756 allocations per 100 records**. The switch loop was
33.7% of that.

| cost | per 100 records | why it ran |
|---|---:|---|
| `nqp::findnotcclass` walked the string grapheme by grapheme | ~5 M | `find_char` segmented even an all-ASCII string with UAX #29 to learn that each byte is one grapheme |
| `nom-ws`'s free `$ws` resolved by name on every call | ~7 M | it is a `package P { my }` lexical `:=`-bound to a list, not a cell, so `trir_seed_outers` refused to cache it |
| `Uni:D` checked through the general type checker | ~4 M | 606 binds of `unjsonify-string(Uni:D \codes)`, ~7,000 instructions each, resolving `Uni` by name through the role and package tables |
| two `Vec`s per `CallTr`, one per generic `nqp::` op, a clone of the call site per `CallGen` | ~26,000 allocations | bookkeeping around the call, not the call |
| `nqp::create` copied its type's name into a `String` | 1,316 allocations | `Symbol::resolve` where `as_str` is `'static` |

## What changed

- **`find_char` scans bytes on a flat string.** A flat index means one ASCII
  byte per grapheme, so the window is a byte range and each byte is its own
  answer.
- **A package lexical is cacheable.** Every write that replaces an entry of
  `package_lexicals` now bumps `unit_lexical_gen`. That covers the four
  `cow_table_mut` sites (now `package_lexicals_cow_mut`) and the plain-value
  branch of the package-scope write-back. So a binding read out of that table
  is as stable under the generation as a cell is. The cache entry also records
  the package it was resolved under, because a package lexical is found by the
  current package.
- **`Uni` and its forms are tag-accepted** in `type_matches_value`, like
  `Int` and `Str`. `NFD:D` still rejects an `NFC`.
- **Call arguments are read in place.** `CallTr` reads the VALUE arguments off
  the top of the two operand banks into the callee frame, then truncates the
  banks. It no longer pops them into two vectors. `NqpOpGen` borrows a spare
  argument vector kept on the TRIR stacks. `CallGen` no longer clones its call
  site, and it keeps a copy of the arguments only when an `is rw` parameter
  needs the read-back.
- **`nqp::create` takes `&'static str`** for its type name.

## Measured

- Per 100 records (callgrind difference, `MUTSU_TRIR_JIT=off`): **107.3 M ->
  91.5 M instructions (-14.8%)**, and **43,756 -> 17,657 allocations (-60%)**.
  The switch loop is now 42.9% of a record.
- `benchmarks/bench-json-fast-spdx.raku`, section time (727 records, release,
  4-core container, warm): `main` 0.098-0.099 s, this change 0.080-0.085 s.
  Rakudo takes 0.044-0.046 s on the same box. These are local numbers. The
  bench CI series is the one to quote.

## Pinned by

- `t/vm/codegen/adr0116-trir-package-outer.t` checks a package-lexical free
  variable read before and after another routine assigns it, `Uni:D` / `NFD:D`
  binds of every form, and `findcclass` / `findnotcclass` windows on flat and
  non-flat strings. TRIR on, TRIR off and rakudo agree, and all four routines
  are accepted.
- The `find_char_on_a_flat_string_scans_bytes` unit test compares the byte scan
  against a direct search, including a string long enough to come from the
  index cache.

Found on the way, and filed as #9238: re-binding a `package P { my $x := ... }`
lexical with `:=` from a sub dies with "Cannot assign to an immutable value".
It does so with TRIR off too.
