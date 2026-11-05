# The tree-walk carrier of `call_sub_value` is deleted

`call_sub_value` ran a code object that had neither `compiled_routine` nor
`compiled_code` by recompiling its AST body through `eval_block_value`, with a
same-name env merge and an exit write-back (ADR-0055 §8.3, #14572). Every Rust
synthesizer of a `Sub` now hands out bytecode (the `substr-rw` Proxy, scheduler
cue thunks, block phasers, `Parameter.default` / `.constraints`, `^add_method`
and class-body routines, exported-method sub forms), and a census over every
`t/` file and every whitelisted roast file finds no call that reaches the
carrier. The branch (about 650 lines) is replaced by an error for a code object
without a body, and the code only it used (`routine_reads_args_array` /
`_hash`, `is_match_var_env_key`, two `RwWritebackSlots` methods,
`SubData::deprecated_message`) goes with it.
