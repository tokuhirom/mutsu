# A module's exported `our` arrays no longer slow every closure

`Math::Sequences` exports about 300 arrays named like `@A000793`. Loading it
made unrelated code in the importer, and the module's own subs, several times
slower, and pushed the distribution's `t/OEIS.rakutest` past its own 10-second
per-sequence timeout. The timed-out worker kept running after the test moved
on, so the file failed at random: an extra `ok` past the plan, or a
"Too deep recursion" flunk (#19324).

Two costs scaled with the number of names a module exports:

- The key classifier treated an uppercase initial after an `@`/`%` sigil as a
  type name, so `@A000793` and its package key `@Math::Sequences::Integer::A000793`
  were "system names". Every closure capture kept all of them, and every
  `.map`/`.grep` plan copied them. They are now plain user variables, captured
  only when a closure names them. Sigil-less keys, `&` routines and
  pseudo-stash spellings (`@MY::x`, `@CALLER::x`) keep the old classification.
- A module mainline's own `our` writes reached the retain-on-miss caller-var
  writeback set under their package keys. No frame ever claims those names, so
  every later call return checked all of them, each with a linear scan of the
  frame's locals. The names are no longer recorded. The drain also looks pending
  names up through the chunk's name index now, so the drain costs
  O(min(pending, locals)) hash probes instead of O(pending × locals) string
  compares.

With 1000 exported arrays, a closure call in the importer took ~375 µs and now
takes about the same ~4 µs as without the import. `@A000793[^48]` drops from 17 s
to 7.5 s, and `t/OEIS.rakutest` no longer times out.
