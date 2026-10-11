# A token body is compiled once, not once per parse

`Grammar.parse` evaluates the start rule's body (`eval_token_def`), and a call to a
parameterized token evaluates that token's body for the call's arguments
(`resolve_one_token_pattern_with_args`). Both went through the uncached
`eval_block_value`, so each `.parse` compiled the declaration body from its AST again.

Both now go through `Interpreter::eval_decl_body_value`, which keys the carrier-compile cache
by the registration (`CarrierCacheKey::Decl(decl_order)`; `insert_token_def_in` stamps a fresh
`decl_order` per registration, so one key is one body). For the program in #10266 the
`carrier-compile:` line reads `misses=3 uncached=0` at both 10 and 60 parses; before it read
`uncached=N+1`. Pinned by `tests/token_body_compile_cache.rs`.

Measured with callgrind on a release build (second run of each, so the module cache is warm), 200
parses of that program: 120,238,733 -> 112,292,834 instructions (-6.6%, about 40k instructions per
parse). The wall clock was not measured; this does strictly less work.
