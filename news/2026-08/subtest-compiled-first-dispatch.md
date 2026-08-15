# `subtest "name" => { ... }` no longer recompiles its block from AST on every call

The common test-file idiom `subtest "name" => { ... }` — `subtest` called as
an ordinary function taking a `Pair` whose value is an anonymous block —
resolved through `test_fn_subtest` → `call_sub_value` → `eval_block_value` →
a **fresh `Compiler::compile()` call**, the same re-entrant, EVAL-like
compilation path `EVAL`/embedded regex `{...}` blocks use. So every single
`subtest { ... }` call parsed/compiled the block's AST from scratch, not
just once — for a class declared inside such a block (the common `plan N;
class C {...}` shape), this also re-triggered a full runtime method-body
compile on every call.

The block never actually arrives as bare AST needing a first compile: the
main-pass compiler already compiled the anon block into bytecode
(`OpCode::MakeAnonSub`), and the resulting `SubData` carries `compiled_code`
— `call_sub_value`'s compiled fast path just never looked at it for a
closure (only for `compiled_routine`), so the carrier fell back to
recompiling `data.body` by hand every time.

Fixed with the same compiled-first lever already used for `reduce`/`produce`
(#5942/#5944): `test_fn_subtest` now dispatches a `Sub` carrying bytecode
through `vm_call_on_value` — the same well-tested path an ordinary `b()`
call takes — instead of the AST carrier. A `Sub` without bytecode (built
interpreter-side) keeps the carrier unchanged.

Verified with `MUTSU_VM_STATS=1`: a 50-iteration `for ^50 { subtest "s" => {
class C {...}; ... } }` loop's `method_body_runtime_compiles` counter drops
from 50 to 0.
