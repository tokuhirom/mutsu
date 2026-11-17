# Sized native int stores and `is rw` accessor assignments do less work

Two costs of C-port style Raku code, found in Timezones::ZoneInfo's `mktime`/`localsub`
(#15079):

- **A store into an `int32`/`int16`/`uint8`/... lexical** ran the generic type match and the
  constraint coercion on every assignment. Only `int`/`int64` skipped them before. Every native
  int type admits any `Int`, and the coercion is the identity for it, so a store of an `Int` now
  goes straight to the overflow wrap, which still runs. In `posix-from-calendar` this cut the
  generic matches from 102k to 14k per 10 calls.
- **`$obj.attr = v` through a generated `is rw` accessor** cloned every attribute definition
  of the class's MRO to find the one being assigned, then dropped the clones: ~15k
  instructions per assignment. It now asks `find_public_class_attribute`, which gives the same
  answer without cloning anything. The full list is built only on the error path, to name the
  type in a refusal.

Together they take 9% of the instructions out of `posix-from-calendar`. The wall clock barely
moves: the remaining cost is the generic sub-call floor. A sigil-less parameter
(`sub isleap(\year)`) costs ~5 µs per call, against ~1 µs for `$year`.
