# A `use`-loaded sub no longer loses its tail value to a statement `with` block

Resolved 2026-07-29. What the ticket described as "a `use`-loaded sub returns
its `with`-block value instead of the tail variable" turned out to be a chain
of four independent compiler/VM gaps plus two NativeCall marshalling holes, all
surfaced by DBDish::Pg's bytea path (`escapeBytea` / NativeHelpers::Blob's
`blob-from-pointer` / `str-to-blob`). Fixing the chain took DBIish's upstream
`t/36-pg-blob.rakutest` from 12/17 to 17/17 (raku parity), and the Pg suite
from 6/11 to 7/11 files matching raku.

The chain, in dependency order:

1. **A statement `given` had an indeterminate stack effect** — `exec_given_op`
   pushed the body value only when the body left one, so statement-position
   compilers could not pair it with a `Pop`. It now always nets exactly one
   stack value (Nil when the body produced none), mirroring
   `exec_do_given_expr_op`.
2. **Statement-position `given` values leaked past their statement.** `with X
   {...}` lowers to `if X.defined { given X {...} }`; the branch's `Given`
   value survived the `if` and, on the `eval_block_value` call path (whose
   result is `stack.last()`), shadowed the enclosing block's real tail value —
   only for `use`-loaded subs, because only those dispatch through
   `call_function_fallback`. Statement loops (`compile_body_with_implicit_try`,
   `compile_stmts_value`, the routine/closure body loops) now pop it.
3. **`compile_phaser_block_scope` collapsed every non-`Expr` tail statement to
   `True`.** A module sub with a `LEAVE` phaser and a tail `with`/`if`
   (`str-to-blob`'s exact shape) returned `True` instead of the branch value.
   The tail statement is now compiled in value position
   (`compile_tail_stmt_value`, mirroring `Compiler::compile`'s tail arms:
   if/given/call/block/decl/assign).
4. **A `given`/`when` body whose tail is a statement-form `Stmt::Call` yielded
   Nil.** The parser resolves a known routine name (e.g. a sub imported by an
   already-parsed `use`) to a statement call; `compile_when_tail_stmt` had no
   arm for it and fell back to sink compilation. This is why
   `with ptr { blob-from-pointer(ptr, :$elems, :$type) }` returned Nil inside
   a module while the same code worked in a mainline script.
5. **`nativecast(Str, $ptr)` produced an opaque handle tagged `Str`** (empty
   when stringified) instead of reading the pointer as a NUL-terminated C
   string — so `escapeBytea` inserted garbage into bytea columns.
6. **A native METHOD's `is rw` numeric out-parameter never reached the
   caller's variable.** The sub-form fix (#5548) keyed off `VarRef` arguments
   at the VM call site, but method arguments arrive by value; the caller name
   is now resolved from the dispatching CallMethod op's arg-source list and
   queued through `pending_rw_writeback_sources`.

Pins: `t/with-statement-tail-value.t` (+ `t/lib/WithTailVar.rakumod`,
`t/lib/WithTailHelper.rakumod`), `t/nativecall-method-rw-out-and-cast-str.t`
(sqlite3-driven, skips without libsqlite3).

Original repro, for the record:

```raku
# TAIL5.rakumod
unit module TAIL5;
sub y2(\ptr) is export {
    my $b = 42;
    with ptr { 2.so; }
    $b;
}
```

```raku
use TAIL5;
say y2(5);      # raku: 42    mutsu (before): True
```
