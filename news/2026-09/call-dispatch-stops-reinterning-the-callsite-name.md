# The general call dispatch path stops re-interning the callsite name

`exec_call_func_op_inner` and `dispatch_func_call_inner` (`src/vm/vm_call_func_ops.rs`)
re-hashed the same callsite name several times per call: `has_proto_cached`,
`has_declared_function_cached`, `has_multi_function_cached` and
`fn_base_name_registered` all took `&str` and called `Symbol::intern`
internally, even though every hot caller already held the name's
pre-interned `Symbol` (`CompiledCode::const_sym`). `normalize_call_args_for_target`
(`src/vm/vm_call_helpers.rs`), which runs on every dispatch that reaches the
general named binder, called all four of them in a row.

Added `*_sym` variants of the four predicates that take the `Symbol`
directly — the `&str` entry points now delegate to them — and converted
every call site in those functions that already held one, including
`normalize_call_args_for_target` itself (now threaded a `name_sym`
parameter) and `dispatch_func_call_inner` (now takes the callsite `Symbol`
as a parameter instead of interning its own name on entry).

Measured with the deterministic `Symbol::intern_calls()` counter
`tests/named_call_intern_budget.rs` and `tests/routine_package_switch_budget.rs`
use: a call taking the general named binder (native `str`/`int` parameter
types, which the light-call fast paths reject) dropped from about 13 interns
per call to about 6; a `where`-constrained single-candidate call dropped
from 53.0 to 46.0; a named multi call dropped from 15.0 to 3.0. Both budget
tests' limits were tightened to match.

While investigating why one of those tests' documented residual (a
qualified light call into a module sub, `tests/routine_package_switch_budget.rs`)
did not move, found a distinct cause — `push_routine_with_location`
re-splits and re-interns a qualified callsite's short name on every call —
and filed it separately as
[#8776](https://github.com/tokuhirom/mutsu/issues/8776) rather than folding
an unrelated fix into this change.

Fixes [#8690](https://github.com/tokuhirom/mutsu/issues/8690).
