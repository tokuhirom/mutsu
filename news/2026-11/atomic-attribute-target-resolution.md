# An atomic op on an attribute stops walking the env

`cas $!head, -> $next { ... }` and `$!elems⚛++` find their target by name. Two
steps of that lookup cost far more than the operation itself.

- `canonical_atomic_var_name` handles a name the frame env does not bind by
  searching the whole env for a variable whose value is identical to the
  target. An attribute spelling (`!head`, `.count`) is never in the env, so
  every attribute atomic op paid that walk, about 2k instructions each. An
  attribute names the invocant's attribute cell and never a lexical, so the
  spelling is now its own canonical name.
- `self_attr_cell_target` built the declaring class's qualified key
  (`Owner\0head`) with a `format!` on every call. It now uses the memoized
  `qualified_attr_key` and returns the key as a `Symbol`.

On Concurrent::Stack, one push plus one pop (two `cas` calls and two `⚛`
updates on attributes) fell from 231.2k to 224.9k instructions after the first
two steps (callgrind slope, profiling build). The alias search itself went from
9.9k to 0.9k per pair. `t/stress.rakutest` takes 6.5-7.3 s against rakudo's
1.6-2.0 s, so #11482 stays open. Most of what remains is the two closure
calls (~100k per pair) and the method calls around them.
