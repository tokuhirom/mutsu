# Precomp cache: deterministic upvalue order

The nightly `precomp-verify` job failed (#12606): `Test::Tap` compiled to a
different `upvalue_parent_slots` order in different processes, so a warm run
under `MUTSU_PRECOMP_VERIFY=1` exited with status 70 on every `S17-supply` test
that loads it. The extra upvalue indices for free `&name`s a closure only hands
on to nested closures were assigned by iterating a `HashSet`; they are now
assigned in name order, so the cached bytecode is independent of the per-process
hash seed.
