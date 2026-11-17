# The hyper/race shared-key rollback is gone

A synchronous `hyper`/`race` op used to snapshot the keys of the cross-thread
lineage store before starting its batch threads and retract every key they
added after the join (`shared_var_keys_snapshot` / `retain_shared_var_keys`).
The batch threads published the lexicals they read there, and a stale entry
would have shadowed a later op's fresh binding of the same name.

Since ADR-16595 a lexical never enters that store (#17837), so there was
nothing left to retract except, when the spawning thread was the root lineage,
genuine process-wide entries (a `state` cell or an atomic lane first created
inside the op). The rollback and the store's `retain_own` are deleted (#9939);
`t/vm/scope/hyper-sequential-ops-fresh-lexical.t` pins the behaviour it used
to protect.
