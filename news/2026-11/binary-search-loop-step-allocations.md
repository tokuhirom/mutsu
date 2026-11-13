# The sorted-insert loop step stops allocating and walking the env

One step of Array::Sorted::Util's duplicate scan (`cmp($needle, haystack.AT-POS($i))` through a `:&cmp = &[cmp]` parameter, compared with `Order::Same` by `nqp::eqaddr`) did several things per execution that it did not need to:

- every `cmp`/`<=>`/`leg` answer allocated a fresh `Order` enum box; the three values are now built once per process;
- every `($i = ...)` expression allocated a `String` for its container tag; the tag now carries the name's interned symbol;
- the `Order::Same` bareword memo re-checked the whole env chain, static links included, to confirm nothing shadows it; a symbol no env tier has ever held (a new `EVER_TIER_KEY` latch) is now answered from the base tier directly;
- a core operator called through its routine value re-derived the routine's base name and went through `call_infix_routine_in`'s prologue on every call; the base name is memoized per symbol and the call goes straight to `infix_pair`, which tests `cmp` first;
- an interpreter row on the method-site lane (`haystack.AT-POS($i)`) copied its arguments into a heap vector.

Around the calls, a sigilless parameter's alias chain is followed on borrowed names instead of cloning each into a SipHash set, and a call's argument-source names are shared with the binder instead of copied.

Callgrind, profiling build: the isolated 64-step scan went from 496.5M to 445.2M instructions for 600 scans (-10.3%), and one push of Concurrent::PriorityQueue (t/02's key distribution, 32 duplicates) from 1.209M to 1.179M instructions (-2.5%). Wall clock on the 4-core box did not move measurably. Refs #15786.
