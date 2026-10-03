# atomic-fetch-sub and atomic-sub-fetch

`atomic-fetch-sub` and `atomic-sub-fetch` were listed as compiler-special calls but never
compiled, so any use died with "Unknown function". They now compile to the existing atomic add
helpers with the delta negated, so there is still one read-modify-write implementation.
Found by Cache::Async's `t/05-monitoring.rakutest`, which now passes under mutsu.
