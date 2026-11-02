# The strict pragma's env key is built once

`Interpreter::env_is_no_strict` runs on every variable read that misses the other stores (through
`no_strict_package_var`) and looked up the `strict` pragma mark with
`MetaNs::Pragma.key_for_str("strict")`, which interns the constant name -- a string hash -- on each
call. A scripted `rust-gdb` sample of `Symbol::intern` inside the AttrX::Mooish loop (240 samples)
put it at the top of the intern callers. The key is now built once (`pragma_key()`, a
`OnceLock<Symbol>`).

`scripts/call-path-ab.sh mooish-lazy`, profiling build, callgrind slope per iteration: `Symbol::intern`
254k -> 245k Ir (-9k), the rest of the loop unchanged. Small by itself; the same sample ranks the
other `key_for_str(&str)` callers (`update_bound_decont_marker`, `sigilless_readonly_key`,
`no_strict`-adjacent `get_our_var`) as the next ones, each a few percent of the intern traffic
(~1,600 interns per iteration in total).
