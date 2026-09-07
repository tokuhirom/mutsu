# `nqp::` ops and `say`'s Str gist stop paying for the general call machinery

Sixth perf slice for `todo/deep/vendor-real-test-module.md`. Callgrind on the
per-assertion protocol (300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`,
one-assertion baseline subtracted, release build) put two things at the top of
what a `Test.rakumod` assertion does that is not the assertion:

- **An `nqp::` op paid ~4k instructions of call dispatch for a ~1k op.**
  `exec_call_func_op` already had an `nqp::` arm, but it sat at the *end* of the
  general chain: the increment-operator gate, the NativeCall table, the
  empty-proto veto, the `&`-param shadow set, the direct-mapped and
  name-keyed light-call caches, the OTF cache, `decode_arg_sources`, and
  `normalize_call_args_for_target`'s registry probes (`fn_keys_for_base`,
  `has_proto_cached`) all ran first and all missed, because `nqp::` is a
  reserved namespace no routine can be declared in. `proclaim` runs five of
  these per assertion (`nqp::time` x2, `nqp::join`/`nqp::split` x2, one
  `nqp::iseq_i`), so that was ~20k per assertion of dispatch that could never
  change the answer. The callee symbol now carries a memoized `flags::NQP_OP`
  bit (the same per-symbol flag byte the return merge uses), and the op's
  `CallFunc` takes `exec_nqp_call_op` first: spread `|` positions, unwrap
  `VarRef` captures (what `normalize_call_args_for_target` always chose for an
  unregistered name), strip the callsite-line marker, FETCH `Proxy` arguments,
  dispatch. The JIT's `call_func` shim reaches the same entry.
- **`say $str` rendered its gist through the slow-path method dispatch.**
  `render_gist_value` called `call_method_with_values(value, "gist")` for every
  value, which for a plain `Str` was a ~7k-instruction walk of the
  qualified/mixin/proxy/format/collection probes before the native `Str` row
  answered "the string itself". `$output.say: $tap` is how every TAP line the
  vendored module prints reaches stdout. A `Str` now gists as itself directly,
  gated on the same memoized `native_lever_a_user_override` check the native
  method fast paths use, so an `augment class Str { method gist {...} }` still
  dispatches.

Nothing is special-cased for `Test`: both paths are what any program that
calls `nqp::` ops or `say`s a string was paying.

## Measured

Callgrind, per assertion, baseline subtracted (the profile's
`exec_call_func_op'2` row is the five `nqp::` calls;
`try_native_io_handle_output` is the `$output.say`):

| | before | after |
| --- | --- | --- |
| per assertion | 335,929 Ir | 313,662 Ir |
| `exec_call_func_op'2` (5 `nqp::` calls) | 28.1k | 16.0k |
| `try_native_io_handle_output` | 9.8k | 3.2k |

**-6.6% per assertion.**

A measurement note for whoever runs this protocol next: the first run after
a rebuild re-parses `Test.rakumod` (the module precompilation cache is keyed
to the binary), so warm the cache with an uninstrumented run before the
300-assertion side, or the ~1G of parser work lands on it.
