# A package stash's `@`/`%` entries are no longer itemized

`package Q { our @a = 1, 2 }; say Q::<@a>.raku` printed `$[1, 2]` where rakudo prints `[1, 2]`.
The same itemization affected `%` entries, `GLOBAL::<@g>`, `Q::{$key}` and `Q::.values`.

A `Stash` stores its symbols in a `Value::Hash`, and `make_stash_instance` built that with
`Value::hash`. That constructor wraps every value as a Hash element container (ADR-0040), so an
Array entry came back as an item. A stash is a `Map` of symbols, though: its `@a` entry *is* the
package's Array. The symbol table is now built with `Value::hash_bare_values`, the constructor
mutsu already uses for a `Map`, so each entry reads back bare. A `$` entry is still its own
`Scalar` (#10757).
