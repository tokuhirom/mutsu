# floor/ceiling/round routine forms handle big Rats and Ints

The function forms `floor($x)`, `ceiling($x)` and `round($x)` carried their own
copy of the rounding logic that only knew word-sized values and returned 0 for a
`BigRat`, `FatRat` or `BigInt`. They now delegate to the shared `Rounding`
routine the method forms use. Found through Net::Ethereum's `t/14.t`
(`fake_exponential`), which now passes.
