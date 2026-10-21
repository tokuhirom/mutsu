# The native method entry's by-name receiver bypasses are measured and mostly gone

`try_native_method_raw`, the VM's fallback after the method table, carried
about twenty-five checks that named a method and a receiver state and either
declined the call or answered it out of band. A liveness probe (log which test
file fires each check, then rerun those files with that check switched off) over
all 8,212 `t/` files and the 1,427 whitelisted roast files sorted them:

- Twelve changed no answer and are deleted: the untouched-`Seq`, `Supply`,
  numeric-bridge (with its per-class probe cache), user-iterator, collection
  `gist`/`raku` and typed-`Array` `raku` declines, the `unique`/`repeated`/`squish`
  `WHICH` warm-up, the `Map` rendering and typed `clone` post-fixes, and the
  core-type-object probe (`Thread.usage` and `ThreadPoolScheduler.usage` are
  rows now).
- The receiver states that only the general entry can read (a `Proxy`, an
  element source behind an iterator the rows cannot pull, a lazy pipe source,
  an enum type object's `pick`/`roll`, a typed `Hash.raku`) are decided once,
  by `method_table::receiver_step::general_entry_owns`, before the table: it
  replaces the table's `early_entry_defers` veto and five name checks.
