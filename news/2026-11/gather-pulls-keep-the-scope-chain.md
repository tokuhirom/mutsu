# Gather pulls and map plans no longer copy the whole scope

After #19324, `Math::Sequences`' `@A000793[^48]` still ran 5.4x slower than
rakudo, past `t/OEIS.rakutest`'s 10-second per-sequence timeout. Two more costs
grew with the size of the program or module scope (#19352):

- A gather pull saved the puller's env with `clone_env`, which flattens a
  scoped chain into one fresh map, and after the pull put that copy back as
  the puller's env. Every pull copied every name in scope, and the fresh tier
  left each per-tier memo cold. In particular, every closure made after a pull
  rebuilt `Tier::capture_sys` over all of the scope's system names. The pull
  now saves and restores the puller's chain itself, which is an `Arc` clone.
- An inline `.map`/`.grep` plan walked every entry of its callback's capture,
  which also merged the capture's shared layers into one map per closure. That
  was ~2,000 keys per plan for a program importing 1000 package arrays. A body
  that names only what it mentions now takes from each shared system-name layer
  only the keys with no subject, plus the ones its mentioned names select,
  through an index memoized on the layer (`Tier::plan_key_index`). A debug-build
  assertion checks that the result equals filtering every entry.

Importing 1000 package arrays now adds ~30% to the strict-partitions loop
(it added 3.5x before). `@A000793[^48]` drops from 12.6 s to ~6.5 s (rakudo:
~2.2 s), and `t/OEIS.rakutest` no longer hits its timeout, even with four
copies running in parallel on four cores.
