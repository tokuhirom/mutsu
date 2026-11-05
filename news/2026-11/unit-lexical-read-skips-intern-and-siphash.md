# A free-variable read of a mainline sub no longer interns its name or SipHashes the sub table

A mainline named sub reading a file-scope variable (`sub run-loop { ... %h<a> ... }`
with `my %h` at file scope) resolves the name through
`Interpreter::unit_lexical_slot`. Two costs in that path scaled with nothing
but a module being loaded:

- `LexicalState::mainline_lexical_subs` was a `std::collections::HashMap`, so
  `active_unit_lexical_bucket` SipHashed the running routine's name (a `String`)
  on every read. It is an `FxHashMap` now, like its neighbours.
- the lexsub latest-cell probe (`lexsub_latest_cell`) interned the variable's
  name again although the read path already held its symbol. `unit_lexical_slot`
  now passes the symbol through (`lexsub_alias_slot_sym`).

- `unit_lexical_slot` probed the bucket table twice per read (once inside
  `active_unit_lexical_bucket` to check the bucket is non-empty, once for the
  cell). `active_unit_lexical_entry` returns the bucket's cells with its key.

Measured with `use Cro::HTTP2::RequestParser` loaded and a `$s += %h<a>` loop in
such a sub (profiling build, loop-only callgrind, slope 3,000 -> 9,000
iterations): 4,413 -> 4,083 Ir/iteration (-7.5%) for the first two changes, then
4,083 -> 4,009 (-1.8%) for the single bucket probe.
