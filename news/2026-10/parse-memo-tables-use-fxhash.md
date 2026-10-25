# The parse memo tables and the RakuAST origin tables hash with Fx

The `primary`, expression and statement parse memos are keyed by `(generation, pointer,
length)` and were hashed with SipHash, which `callgrind` showed as 199,194 `hash_one` calls
(12.8M instructions) on the first 1,500 lines of `roast/S15-nfg/emoji-test.t`. They and the
`MUTSU_RAKUAST` origin tables (`rakuast::expr_origins`, fingerprint included) now use
`FxHashMap` / `FxHasher`. None of them is iterated in an order the output depends on.

`mutsu -c` on that slice (callgrind instructions): ordinary frontend 816.6M -> 802.8M;
`MUTSU_RAKUAST=1` 995.9M -> 961.2M (+22% -> +19.7% over the new ordinary figure). #15011.
