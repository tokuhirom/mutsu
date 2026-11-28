# A module's exported `our` arrays slow closures and call returns far less

`Math::Sequences` exports about 300 arrays named like `@A000793`. Loading it
made unrelated code in the importer, and the module's own subs, several times
slower. That pushed the distribution's `t/OEIS.rakutest` past its own 10-second
per-sequence timeout. The timed-out worker kept running after the test moved
on, so the file failed at random: an extra `ok` past the plan, or a
"Too deep recursion" flunk (#19324).

Three costs scaled with the number of names a module exports:

- The key classifier treated an uppercase initial after an `@`/`%` sigil as a
  type name. So `@A000793` was a "system name", and every closure capture kept
  every such array. Unqualified `@`/`%` names with an uppercase initial are now
  plain user variables, captured only when a closure names them. A
  package-qualified `@Pkg::X` stays a system name, because closures and `start`
  workers must carry package variables they don't name.
- A `.map`/`.grep` plan copied every one of those package keys into its
  per-call capture list. Where the body can only reach names it mentions, the
  plan now drops a non-global package array or hash it doesn't mention, the same
  way it already dropped an unmentioned `&Pkg::routine`.
- A module mainline's own `our` writes reached the retain-on-miss caller-var
  writeback set under their package keys. No frame ever claims those names, so
  every later call return checked all of them, each with a linear scan of the
  frame's locals. The names are no longer recorded. The drain also looks pending
  names up through the chunk's name index now, so it costs
  O(min(pending, locals)) hash probes instead of O(pending × locals) string
  compares.

With 1000 exported arrays, a closure call in the importer took ~375 µs and now
takes ~4 µs, about the same as without the import. `@A000793[^48]` drops from 17 s
to ~12.5 s (rakudo: 2.3 s). The rest of the gap, a capture memo rebuilt over the
whole scope on every gather pull, is #19352.
