# ③ Migrating native IO state to VM ownership (IO handle shared-handle design)

Of **③** in the [PLAN.md](../PLAN.md) final goal "remove the tree-walking Interpreter execution path → eliminate the dual store",
this covers eradicating the **native-method IO** of ledger
[vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md) §1
(the `native_io_*` dispatch for `IO::Handle`/`IO::Pipe`). The parent design is the ③ design
[vm-state-ownership.md](vm-state-ownership.md); the model to follow is ② [vm-registry-ownership.md](vm-registry-ownership.md).

## The problem to solve

The VM's catch-all method dispatch (`vm_call_method_compiled.rs:424` and others) drops methods whose receiver is
`IO::Handle`/`IO::Pipe` into the generic dispatch of `self.interpreter.call_method_with_values` (the §1 native-method fork).
To replace this with VM-native dispatch, the VM needs to be able to touch the **IO handle state**. That state is
currently held exclusively by the `Interpreter`:

- `Interpreter.handles: HashMap<usize, IoHandleState>` (`src/runtime/mod.rs:851`)
- `Interpreter.next_handle_id: usize` (ibid. 852)

An `IO::Handle` value holds a handle id and looks it up in `handles`. `IoHandleState` is a large structure holding
`fs::File`/socket/listener/buffers etc. (`mod.rs:529`). The IO logic itself is ~10k lines (io.rs/native_io.rs/handle.rs/
builtins_io.rs), but **state access (`self.handles`/`next_handle_id`) is localized to only ~36 sites** (7 files).

## Why the ② registry playbook applies as-is (investigation settled 2026-06-11)

env is "hottest, single-owner, already COW", hence Arc<RwLock> is out (③ design §core). But **unlike env, IO handles
are isomorphic to the ② registry**, so the shared-handle scaffold works:

1. **Cross-thread sharing is snapshot, not live.** `clone_for_thread` (`mod.rs:5283-5314`) deep-copies `handles`
   per thread via `try_clone()`, and handles opened in a child thread are merged back to the parent via the `new_handles` Vec +
   `next_handle_id` reconciliation (`builtins_system.rs:133-149`/`1594-1600`).
   = the same as the registry's per-thread snapshot. Therefore the lock is needed not for cross-thread contention but
   only for **within-thread VM↔Interpreter peer access** (non-simultaneous due to the ping-pong).
2. **IoHandleState being non-Clone and blocking IO do not become problems.** The registry could drop the guard
   immediately via Clone (clone-out), but a handle holds an `fs::File` and cannot be Clone, and IO may block. However,
   **because of the per-thread snapshot no other party can hold that thread's lock at the same time** (single-threaded
   execution). So holding the guard across blocking IO does not deadlock. The only danger is **re-entrant re-acquisition
   on the same thread**, which is detected by the same debug runtime guard as in ②.
3. **The existing code already avoids held-across-reentry.** As `handle.rs:159-165` shows, the current code drops
   `self.handles.get_mut(&id)` before calling other methods (`encode_with_encoding` etc.) and re-borrows afterwards
   (currently enforced by the borrow checker). Making it Arc<RwLock> removes the borrow checker's enforcement, but the
   code structure already follows the discipline, and the debug guard catches future violations.

→ **Within ③, IO handles alone are correctly served by the "shared-handle scaffold (② approach)"**. env/types/current_package
are folded into plain fields after fallback eradication (④/⑤), but handles, like ②, go Arc<RwLock> scaffold → folded into
a plain VM field in ④/⑤.

## Representation

```rust
// src/runtime/io_handles.rs
pub(crate) struct IoHandleTable {
    pub(crate) map: HashMap<usize, IoHandleState>,
    pub(crate) next_id: usize,
}
// Interpreter.io_handles: Arc<RwLock<IoHandleTable>>
```

- The re-entry detection guard **promotes ②'s `reentry_check` (thread-local keyed by lock address) into a general-purpose
  module `src/runtime/lock_reentry.rs`**, shared by both registry and IO ("1 operation = 1 implementation"). The panic
  message takes the lock name as a parameter.
- `io_handles()`/`io_handles_mut()` accessors (isomorphic to ②'s `registry()`/`registry_mut()`).
- `clone_for_thread`: per-thread snapshot via `Arc::new(RwLock::new(IoHandleTable { map: <try_clone deep copy>, next_id }))`
  (semantics unchanged).
- `new_handles` merge-back (`builtins_system.rs`): extract open handles from the child's io_handles snapshot,
  merge them into the parent, reconcile `next_id` (route the existing logic through the accessors).

## ★ Crux discovered during implementation (2026-06-11 — the decisive difference from registry)

When starting PR-A, it became clear that the registry's mechanical transformation (`self.FIELD` → accessor) does not
apply as-is:

- **`handle_state_mut(&mut self, h) -> Result<&mut IoHandleState, _>` (`handle.rs:114`) returns `&mut IoHandleState`
  and is used from ~32 sites (handle.rs / native_io.rs) as `let state = self.handle_state_mut(h)?; <use state>`.**
  Once made `Arc<RwLock>`, the `&mut IoHandleState` becomes a reference into the guard, and unless the guard is returned
  alongside, the lifetime is cut off (guard+reference is a self-referential structure that cannot be written safely).
  The registry could drop reads immediately via `.cloned()`, but for IO this is impossible because the **`&mut`-returning
  accessor is woven into the IO logic**.
- Furthermore, `write_to_handle_value_trying` (`handle.rs:165-169`) calls `self.emit_output()` while the `state`
  borrow is live (currently made possible by per-path NLL). Once guard-ified, there are places where a call spanning
  the guard = a re-entry deadlock.

→ **The `Arc<RwLock>` lift is a large-scale refactor: "redesign the `&mut`-returning `handle_state_mut` API into a closure
form (`with_handle_mut(id, |state| …)`) or a guard-returning form, and rewrite the ~32 call sites including a re-entry
audit"**. It is not the light extraction of the registry approach. Therefore the slices are split (below).

## Slice plan (strangler-fig; each PR behavior-preserving; CI is the safety net)

- **PR-A = redesign `handle_state_mut` into a closure-form API (groundwork before the lock)**: replace the `&mut`-return
  with `with_handle_mut`/`with_handle` (the closure receives `&mut IoHandleState`) and convert the ~32 call sites.
  **At this stage everything remains a plain field** (the borrow checker continues to enforce non-re-entry). The
  closure-ification makes re-entry across the guard syntactically impossible, making PR-B's lock-ification safe. At each
  site, audit "does the closure body re-enter another IO call" (paths that re-enter are restructured into a
  guard-drop-equivalent shape). Behavior-preserving.
- **PR-B = lift handles to a shared handle**: generalize `reentry_check` into `lock_reentry.rs`, introduce `IoHandleTable`,
  `Interpreter.handles`/`next_handle_id` → `io_handles: Arc<RwLock<IoHandleTable>>`, route `with_handle_mut`/
  accessors through the guard, follow up `clone_for_thread`/`new_handles` merge/`flush_all_handles`. Behavior-preserving.
- **PR-C = migrate the io_handles handle to the VM** (equivalent to ② #2895) + the first VM-native dispatch (pure-handle methods).
- **PR-D+ = VM-nativize IO method dispatch (the rest)**: keep lowering the catch-all's `IO::Handle` receivers into `try_native_io_*`
  and erase the generic dispatch fork. **Classify the remaining `native_io_handle` methods into 3 tiers by dependency** (the
  starting point for the next session — a detailed survey at PR-C completion):
  - **Tier-1 = additional pure-handle slices (③ state migration *not* required; same shape as PR-C; doable next session)**: the setters
    `chomp`/`nl-out`(method form)/`out-buffer`/`encoding` (`set_handle_encoding` is a pure state mutation) and
    `native-descriptor` (`as_raw_fd`). The `nl-in` getter is Interpreter-dependent only in that on an unopened handle it falls
    back to `default_line_separators()` (which reads `newline_mode`) = requires a small trick. A low-risk slice that proceeds
    by **just adding arms to PR-C's `try_native_io_handle_method`**.
  - **Tier-2 = ③ state migration *required* (the real target of §1 fork eradication; PR-C's simple extraction does not work)**: the output family
    `print`/`printf`/`say`/`put`/`print-nl`/`write`/`flush`/`spurt` (depends on `emit_output`(Stdout)/`stderr_output`) and
    the read family `get`/`getc`/`readchars`/`lines`/`words`/`read`/`slurp`/`split`/`comb` (depends on non-UTF8
    `encode/decode_with_encoding` + ArgFiles via `env` (`@*ARGS`)). Restricted to a File+UTF8 target they are pure, but the
    methods interleave with Interpreter calls, so cutting a narrow "native only for File and UTF8; everything else falls
    through" path requires relocating the read-record helpers plus target/encoding guards. True eradication presupposes
    **the ③ later-stage/④ work that makes emit_output/env/encode reachable from the VM**.
  - **Tier-3 = complex (deferred)**: `open`(reopen)/`path`|`IO`/`Supply`/`DESTROY`/`Str`|`gist`.
- **Final fold (④/⑤)**: once the tree-walk execution path is gone, fold io_handles into a plain VM field.

## Discipline (identical to ② — most important)

Never re-entrantly re-acquire the RwLock guard **on the same thread** (read-while-write / write-while-any is a debug panic).
If a path that re-enters another handle method in the middle of an IO operation arises in the future, drop the guard before
calling. The last line of defense is the make roast timeout (it cannot be caught statically = always verify by execution
with smoke tests + roast).

## Scope / non-goals

- Scope = lift native IO state to a ②-style shared handle and VM-nativize IO method dispatch to erase the
  §1 native-method IO fork. Each PR behavior-preserving.
- Non-goals: folding env/types/current_package into plain fields (④/⑤), rewriting the IO logic itself (it is reused),
  overhauling the concurrency model of `Proc::Async`/sockets (PLAN §8.3), true cross-thread sharing of handles (snapshot preserved).

## Progress

- **Design settled (this doc, 2026-06-11)**: settled that ③ native IO, unlike env, can apply the ② registry approach
  (Arc<RwLock> shared handle) (per-thread snapshot means non-Clone/blocking IO do not become problems). State access is
  localized to ~36 sites.
- **Crux discovered (at kickoff, 2026-06-11)**: the `&mut IoHandleState`-returning `handle_state_mut` API (~32 call sites)
  is incompatible with guard lifetimes, so this is not the registry's light extraction. Split the slices into
  "PR-A = closure-form API redesign (groundwork while still a plain field) → PR-B = lock-ification → PR-C = VM handle
  → PR-D = VM-native dispatch".
- **PR-A complete (2026-06-11)**: removed `handle_state_mut` (returning `&mut IoHandleState`) and fully replaced it with
  the closure-form `with_handle_mut(id, |state| …)` / `with_handle_mut_opt` (falls back to `Ok(None)` when the handle
  is invalid). Converted the 32 call sites. The 4 methods containing self re-entry were restructured into
  extract→drop→re-enter to confine the borrow inside the closure: `write_to_handle_value_trying` (phase1
  payload/bytes_written → phase2 `encode_with_encoding` → phase3 dispatch; Stdout/Stderr's `emit_output` outside the
  borrow), `write_bytes_to_handle_value` (same shape),
  `read_line_from_handle_value` (raw record read inside the closure via `LineOutcome::{Done,NeedsDecode}` → outside,
  `decode_with_encoding`), `read_bytes_from_handle_value` (peek target/own_paths → the ArgFiles `@*ARGS`
  read sandwiched between borrows). **Still a plain field** (the borrow checker continues to enforce non-re-entry) with
  completely unchanged behavior
  (verified byte-identical with raku for say+nl-out / print-nl / latin-1 enc / words / getc / read / seek·tell·eof;
  the 8 whitelisted S32-io files PASS). Next up = **PR-B** (introduce `IoHandleTable` + generalize `reentry_check` into
  `lock_reentry.rs` + `handles`/`next_handle_id` → `io_handles: Arc<RwLock<IoHandleTable>>`, route
  `with_handle_mut` through the guard).
- **PR-B complete (2026-06-11)**: lifted `handles`/`next_handle_id` into `io_handles: Arc<RwLock<IoHandleTable>>`
  (`src/runtime/io_handles.rs`, `map`+`next_id`) = the ② registry-style shared-handle scaffold.
  - **Generalized `reentry_check` into `src/runtime/lock_reentry.rs`**: unified ②'s bespoke `RegistryRead/WriteGuard` +
    `reentry_check` mod into the generic `ReentrantReadGuard<'a,T>`/`ReentrantWriteGuard<'a,T>` (lock name
    parameterized into the panic message). `RegistryReadGuard`/`RegistryWriteGuard` became type aliases (call sites
    unchanged; only `new` gains `, "registry"` = 3 sites). The IO side gets `IoHandlesReadGuard`/`IoHandlesWriteGuard`
    aliases + `io_handles()`/`io_handles_mut()` accessors (isomorphic to `registry()`/`registry_mut()`).
  - **Converted all ~59 sites (10 files) to the accessors**: `self.handles.get/get_mut/insert/keys/values_mut` →
    `self.io_handles()/.io_handles_mut().map.*`, `self.next_handle_id` → the table's `next_id`. **The new helper
    `insert_handle_state(state) -> id`** centralizes `next_id` allocation + insert (folds the duplicated
    `let id = next_handle_id; next_handle_id += 1; … handles.insert` across the io.rs/handle.rs/socket sites. Because
    state includes `&self`-dependent parts such as `default_line_separators()`, the discipline is to **construct it fully
    before acquiring the guard** and then pass it in).
  - **`with_handle_mut`/`with_handle_mut_opt` go through the write guard** (PR-A's closure confinement makes self
    re-entry impossible even while holding the guard across the closure = with the per-thread snapshot there is no
    deadlock even across blocking IO).
  - **`clone_for_thread`**: per-thread snapshot via `Arc::new(RwLock::new(IoHandleTable { map: <try_clone deep copy>, next_id }))`
    (semantics unchanged). **Merge-back** (both spawn/await sides in `builtins_system.rs`) extracts open handles from the
    child's `io_handles()` snapshot into the parent's `io_handles_mut()`, reconciling `next_id`.
  - **Re-entry safety**: the debug runtime guard (keyed by lock address) turns read-while-write / write-while-any into an
    immediate panic. In a debug build (guard enabled), verified the smoke tests (say+nl-out / file
    get/lines/words/seek/tell/eof/getc / :w print+say / merge-back of a handle opened by a thread / `$*OUT`)
    byte-identical with raku; whitelisted S32-io (seek/tell/open/
    slurp/spurt/out-buffering/utf16/null-char + IO-Socket-INET/UNIX/accept-threads/fail-invalid/pipe/note/other +
    Async/Async-UDP/signals) + t/ (io-*/socket/thread/concurrency/proc-async/subtest-threaded) green, zero re-entry panics.
    `make test` (cargo 458 + prove 6292) green. Next up = **PR-C** (migrate the io_handles handle to the VM, equivalent to ② #2895).
- **PR-C complete (2026-06-11)**: migrated the `io_handles` handle to the VM (equivalent to ② #2895) + the **first VM-native
  IO dispatch slice**. For ② registry, VM direct-access sites (package_stubs) already existed, so #2895 could be a pure
  handle addition; but IO has **zero** VM direct accesses (the catch-all drops everything into
  `self.interpreter.call_method_with_values`). So adding only the handle would be dead_code → like #2895, make it a
  "handle migration accompanied by real users" and implement the **pure-handle methods** as the first native dispatch.
  - **Extracted the pure logic into `impl IoHandleState`** (`handle.rs`): `flush_buffer`/`tell`/`eof`/`seek`/`close`/
    `is_opened`/`is_tty` — state-exclusive operations independent of emit_output/env/encoding. The Interpreter's
    `*_handle_value` wrappers delegate to these (`with_handle_mut(hv, |s| s.tell())` etc.); native_io's `t`/`opened` also
    delegate. `flush_file_handle_buffer` (an Interpreter associated function) → `IoHandleState::flush_buffer` (6 call
    sites converted). = "1 operation = 1 implementation".
  - **VM handle**: `Interpreter::io_handles_handle()` (Arc clone, same shape as `registry_handle()`); the VM gets an
    `io_handles` field (cloned in `VM::new`) + an `io_handles_mut()` accessor. Both handles point at the same RwLock,
    and the debug re-entry guard detects deadlocks. Since `clone_for_thread` creates a fresh Interpreter, the Arc is
    stable for the VM's lifetime (same rationale as #2895).
  - **`try_native_io_handle_method`** (`vm_call_method_compiled.rs`, immediately before the catch-all): when the receiver
    is a **strict `IO::Handle` match** (excluding `IO::Socket::INET`'s socket-close / `IO::Pipe`'s process-reap close),
    the method ∈ {close/tell/eof/seek/opened/t}, and the arguments are pure (junctions fall through to interpreter
    autothreading), it looks up the state via the VM's `io_handles_mut()` and calls the shared methods. seek's arg
    interpretation matches native_io exactly (non-Int offset→0, whence strings→0/1/2). Everything else returns `None`
    (fall through). `handle_id_from_value` was made `pub(crate)`.
  - **Verification**: PR-B smoke + the new `t/io-handle-pure-methods.t` (16 tests, tell/eof/seek/opened/close/t)
    byte-identical with raku; whitelisted S32-io (seek/tell/open etc. pass through the VM-native path) + socket/pipe/thread
    green; `make test` (cargo 458 + prove 6338) green; zero re-entry panics. The difference where double-close returns
    False (raku returns True) is **pre-existing behavior from before the extraction** (verbatim extraction means PR-C is
    behavior-neutral; roast close.t/open.t also green). Next = **PR-D** (progressively VM-nativize the heavy IO methods
    like read/write/lines/slurp. Being dependent on emit_output/env/encoding, they presuppose the ③ later-stage/④ state migration).
- **PR-D Tier-1 complete (2026-06-11)**: the state-exclusive setters/getters `chomp`/`nl-out`/`out-buffer`/`encoding` +
  `native-descriptor` to VM-native dispatch. Added the pure logic to `impl IoHandleState` (`chomp_setting`/
  `nl_out_setting`/`out_buffer_setting`/`encoding_setting`/`native_descriptor`); the Interpreter's native_io handlers and
  `set_handle_encoding` delegate to them ("1 operation = 1 implementation"). Added arms to `try_native_io_handle_method`
  (for encoding, the Nil↔bin Value shaping is done inside the arm = exact match with native_io; out-buffer's
  `parse_out_buffer_size` was made `pub(crate)`).
  - **★ Discovered and fixed a PR-C shadowing bug (this slice's crux)**: PR-C's `try_native_io_handle_method` was placed
    **immediately before the catch-all**, but `try_compiled_method_or_interpret`/`try_compiled_method_mut_or_interpret`
    have an **`is_native_method` early-fallback gate at their head**, and IO::Handle's native methods (tell/seek/close…
    all of them) dropped through that gate into `call_method_with_values` and never reached L430 = **PR-C's dispatch was
    entirely dead code** (`t/io-handle-pure-methods.t` was green because the fallback result was identical). Fix: moved
    the native-IO dispatch **immediately before the early gate** (both mut/non-mut paths) and removed the dead placement.
    Confirmed via `MUTSU_VM_STATS`: native-descriptor.t has 0% method-call fallback, and in io-handle-pure-methods.t
    tell/seek/close/eof/opened/t are all nativized (PR-C **effective for the first time**). **Lesson: always confirm native
    dispatch is effective via the `MUTSU_VM_STATS` method-fallback counters** (green tests alone cannot detect shadowing).
  - **Behavior-preserving**: the verbatim encoding names (mutsu `latin1` / raku `iso-8859-1`) and out-buffer default 0
    (raku 8192) are **pre-existing differences from before the change** and out of scope. New `t/io-handle-tier1-methods.t`
    (14 tests) 14/14 green on both mutsu/raku (the encoding-name difference is absorbed by matching the setter return with
    the getter). Whitelisted S32-io (native-descriptor/out-buffering/open/
    seek/tell/slurp/spurt/io-special/note/null-char/other + socket/pipe/procasync) green, zero re-entry panics.
- **PR-D Tier-2a complete (2026-06-11)**: **File+UTF8-target text output** `print`/`put`/`say`/`print-nl` to VM-native
  dispatch. **The investigation settled that the output family splits into 2 lineages of dependencies**:
  - **Branch by write destination**: **Stdout**→`emit_output` (depends on the `output` buffer / `output_emitted` / TAP
    subtest depth / thread-clone shared output = **unreachable from the VM**; true eradication presupposes the ③
    later-stage/④ state migration), **Stderr**→the `stderr_output` buffer (same), **File**→direct write to handle state
    (including buffering, **pure handle state**; only non-UTF-8 re-enters `encode_with_encoding`). **Socket** is also
    separate. → **Only File+UTF8/bin can be nativized purely.** The most frequent case, Stdout, stays put.
  - **Payload construction** is `self.interpreter.render_str_value`(print/put)/`render_gist_value`(say) = **the same code
    as the current native_io handlers** and **the VM's own exec_say_op/exec_print_op use the same helpers**, so byte
    parity, zero parity risk, no env desync (the VM's say already calls them likewise).
  - **Writing**: extracted the File branch's buffering into `IoHandleState::write_file_payload` (the File branch of
    `write_to_handle_value_trying` delegates = 1 operation 1 implementation). Added `can_native_text_write` (File+utf8/bin
    gate)/`native_text_write` (closed check + nl_out append + bytes accounting + write)/`native_print_nl` to
    `impl IoHandleState`. New VM `try_native_io_handle_output`
    (**immediately before the early gate**, both mut/non-mut paths). **The gate (target/encoding) is read before payload
    construction** = on fall-through the argument stringification is not executed twice. Junction arguments fall through
    to autothreading.
  - **Verification**: new `t/io-handle-tier2a-file-output.t`(12) 12/12 on both mutsu/raku. Via `MUTSU_VM_STATS` File output
    is native (in out-buffering.t, print/say/put disappear from the fallback, leaving only open/slurp/**flush**);
    Stdout/Stderr/latin1-File correctly fall through (say/print remain in the fallback). io-handle.t's not-ok count is
    identical to main (24 = pre-existing; this change is neutral).
    `make test` (cargo 458+prove) green. **The File branch of `write_bytes_to_handle_value` has different semantics without
    out-buffer support, so it is left un-delegated** (`write`/`spurt` are Tier-2b scope).
- **PR-D Tier-2b complete (2026-06-11)**: VM-nativized `printf` (File+UTF8) and `flush` (all targets).
  - **printf**: added `Kind::Printf` to `try_native_io_handle_output`. The payload is built with the pure
    `crate::runtime::sprintf::{validate_sprintf_directives, format_sprintf_args}` (`mod sprintf` is `pub(super)` = crate-visible;
    identical to the current handler) and written via the File branch (newline=false, same shape as print). A junction
    first argument falls through entirely to the interpreter's threading. Validation errors propagate as
    `Some(Err)` (matching the handler).
  - **flush**: target-independent and pure (`flush_buffer` + `file.flush`; Stdout/Stderr are a no-op with no file).
    Extracted `IoHandleState::flush_for_method`; the interpreter's `flush` handler delegates. Added a
    **dedicated arm** to `try_native_io_handle_method` (right after the junction check, before the Op enum): if the id is
    in the table, native `Bool(true)`; **if absent, `None` and fall through** (the path where the interpreter shapes an
    `X::IO::Flush` Failure as a value cannot be reproduced by the generic Err-on-absent path).
    Closed-but-in-table is Bool(true) = matching the interpreter's existing behavior (raku is Failure = pre-existing
    difference, out of scope).
  - **Verification**: new `t/io-handle-tier2b-printf-flush.t`(9) 9/9 on both mutsu/raku. In out-buffering.t, **flush
    disappears from the fallback** (flush=15→0); printf is also native. S32-io (out-buffering/open/io-special/slurp/spurt) green. `make test` green.
- **PR-D Tier-2c complete (2026-06-11)**: VM-nativized `write`/`spurt` (raw byte writes to File targets).
  - **Sharing the raw write**: extracted the File branch of `write_bytes_to_handle_value` (the out-buffer-less
    `file.write_all`) into `IoHandleState::write_all_to_file` (the interpreter delegates). For the VM, added `is_file_target`
    (File gate, encoding-independent) and `native_write_bytes_file` (closed/mode-Read checks + bytes accounting +
    `write_all_to_file` = the File-only fusion of write_bytes' phase1 + File branch) to `impl IoHandleState`.
  - **Byte construction** is identical to the current handlers: `write` = for each argument, if it is a buffer type
    (Buf/Blob/utf8/utf16) then `supply_chunk_to_bytes` (utf16-capable, made `pub(crate)`); non-buffers are concatenated
    via `render_str_value` (UTF-8 bytes). `spurt` = Buf via `VM::extract_buf_bytes`, Str as
    UTF-8 bytes. New VM `try_native_io_handle_byte_output` (right after the output dispatch, both mut/non-mut paths).
  - **Gates**: write is File-only (encoding irrelevant = raw bytes). spurt is File and, **when the argument is a Str, encoding
    must be UTF-8** (a non-UTF8 Str re-enters `encode_with_encoding`, hence fall through). Buf spurt is fine with File-only.
    Junction fall-through.
  - **Verification**: new `t/io-handle-tier2c-write-spurt.t`(8) 8/8 on both mutsu/raku. Via MUTSU_VM_STATS, `$fh.write`/`$fh.spurt`
    are native (latin1-Str spurt falls through with spurt=1; closed write dies). S32-io (spurt/out-buffering/open/io-special/
    slurp/null-char/**utf16**) green = validates the utf16 buffer write path. `make test` green. **Note: `IO::Path.spurt`
    (receiver is a Path) is a different method and out of scope** (spurt.t's spurt fallback is this).
  - **Next = PR-D Tier-3 / the true main battle**: all remaining `IO::Handle` methods depend on interpreter state:
    - **Stdout/Stderr output** (the non-File branches of print/say/put/printf/write/spurt) = `emit_output`/`stderr_output`
      (output buffer/output_emitted/TAP/thread-clone).
    - **Read family** get/getc/readchars/lines/words/read/slurp/split/comb = ArgFiles via `@*ARGS`(env) + non-UTF8 `decode_with_encoding`.
    - True eradication of these presupposes **the ③ later-stage/④ state migration that makes emit_output/env/encode
      reachable from the VM** = the final stage of native IO removal.
    - Tier-3 = open(reopen)/path/Supply/DESTROY/Str/gist. Plus the small remainder of the **Tier-1 nl-in getter**
      (the newline_mode fallback when unopened).
