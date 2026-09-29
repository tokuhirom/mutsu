# Seq positional binding failover

A Seq no longer binds directly to an array variable. When passed to an array parameter, it now takes a deferred List view, so bounded reads of a Seq backed by an infinite gather do not drain the source before the routine starts.
