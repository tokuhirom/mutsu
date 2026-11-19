# A closure run in another thread's routine context reads that context's key index

Since #18179 a closure resolves its by-name calls through the routine tables
of the execution context it was written in: run on another thread, the call
installs the creator's tables and puts the caller's back afterwards. The
per-interpreter base-name key index (`fn_keys_by_base`) and the
`fn_resolve_gen` mirror were left behind, still describing whichever tables
had been installed before. A module routine whose block declares inner subs,
read through a lazy module array from a `start` block, indexed an inner sub
over one map and then looked it up over the other; the call died with
"Unknown function: find-factor" (a debug build panics in the index audit).
That was the Math::Sequences `t/OEIS.rakutest` regression of nightly sweep
#109.

Entering a foreign routine context now sets the caller's key index aside and
starts an empty one; leaving puts it back. Both swaps re-mirror
`fn_resolve_gen` from the installed functions map's version, so the memos
tagged with it follow the tables (#18258).
