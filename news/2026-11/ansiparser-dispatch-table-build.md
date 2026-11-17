# Terminal::ANSIParser builds its dispatch tables 2.9x cheaper with `use Test` loaded

`make-ansi-parser` fills sixteen 256-slot tables with `$dispatch[$byte] := &action`
binds. Three costs made it 91.6M instructions per parser once `use Test` was loaded
(55.5M without it), and now it is 32M either way (#17429):

- A `&name` read of a block-declared `my sub` asks whether the name has `multi`
  candidates. That `&self` probe used the base-name key index only when the index already
  held an entry for the name, and walked the whole functions map otherwise. A `my sub` is
  re-registered on every call of its enclosing routine, which marks its base name dirty,
  so every read paid the walk, and `use Test` alone puts ~400 routines in that map.
  `peek_fn_keys_for_base` now answers from a complete index without a scan: the entry,
  no keys for a clean base name, and the qualified-name tail index's keys for a dirty one.
- The `$d[$i] := ...` store lane served neither a readonly `for` alias of the table
  (`for @actions -> $dispatch`) nor a readonly `&code` source with no container
  (`my &f := { ... }`). Both now take the lane and store exactly what the generic path
  stores: the element of the alias's Array, and the code object as a read-only element.
- The source-name list every indexed `:=` hands the store was rebuilt by a `MakeArray`
  per bind. It is all literals, so it is now one compile-time constant.

`t/03-csi.rakutest` went from ~16 s to ~9 s on a release build (rakudo: ~2.7 s).
