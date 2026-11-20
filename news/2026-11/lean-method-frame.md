# A planned plain method call runs in a lean frame

The method-call plan (ADR-15737 D1) already replayed a plain method call's
resolution and its fast-path decision, but the frame it then entered, the fast
binder `call_compiled_method_fast_in_unit`, still asked on every call about
every feature a method might use: a deprecation notice, `callframe`,
`samewith` and the deferral context, role composition bindings and `?ROLE`,
`nqp::usecapture`, the implicit `*%_`, an `.^add_method` closure's lexical
capture, `state` variables, attribute `is default` registrations, a return
type, a routine or class declared in the body, and every parameter shape.

Each of those answers depends only on the method, its compiled code and the
registry. The plan now decides them once (`lean_frame_eligible`), and a
method that needs none of them runs in a short frame
(`src/vm/vm_method_lean_frame.rs`): the light call frame, the env overlay,
the parameters, the method-class and routine frames, the body and the
attribute reconcile. Untyped parameters and parameters typed with a builtin
value type (`Int`, `Str:D`, `Mu`, ...) qualify; the per-call argument type
check is the only test left, and a refusal hands the call to the general
binder, which raises the error. The body loop is shared with the general
binder (`run_compiled_method_body`), so the two cannot disagree on `return`,
`leave`, `fail` or the JIT entry. The general binder also stopped allocating
an empty role-binding list per call.

Callgrind slope per call (profiling build, loop skeleton not subtracted):
`$o.m0` 11,469 -> 10,538 Ir, `$o.m1($_)` 13,792 -> 12,846, `$o.mt(Int $x)`
14,773 -> 12,871, `method setk($v) { $!k = $v }` 17,219 -> 16,158. AttrX::Mooish
and MCP::Server::Tool::Web do not move: their hot methods take sigilless,
named or `is raw` parameters or create closures, which this first stage of
#18267 leaves to the general binder.
