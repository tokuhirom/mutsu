# The tail of the third method cascade is late rows

The last by-name arms of `dispatch_method_by_name_3` are now late rows of the method table, found by walking the
receiver's MRO (ADR-11276 §9.139). They covered:

- `Seq.Numeric`;
- the `Date`/`DateTime` coercions of the temporal type objects;
- `Thread.run`;
- the `Str` of a list whose elements have a user `Str`;
- `keys`, `values`, `AT-KEY` and `EXISTS-*` on a `Stash`, an object hash, a `Match`, a `Capture` and a `Pair`.

`Supply.signal` is gone. Rakudo has no such method (`signal(...)` is a routine), and mutsu's version built a supply
that never fired.
