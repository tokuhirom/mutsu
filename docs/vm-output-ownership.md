# ③ latter stage / ④ output-sink (emit_output) ownership transfer to the VM

This is **③ latter stage / ④** of the [PLAN.md](../PLAN.md) end goal "remove the tree-walking Interpreter execution
path → delete the dual store". It resolves what **PR-D Tier-2** of the [native IO ownership transfer](vm-io-ownership.md)
revealed: "**File output could be made native with pure handle state, but Stdout/Stderr output depends on
`emit_output` and is unreachable from the VM**". The model to follow is the shared-handle playbook of
② registry / ③ native IO (io_handles) ([vm-io-ownership.md](vm-io-ownership.md) / [vm-registry-ownership.md](vm-registry-ownership.md)).

## Problem to solve

The **Stdout/Stderr receiver branches** of `IO::Handle.say/print/put/printf/write/spurt` write to `emit_output`
(Stdout) / the `stderr_output` buffer (Stderr). These depend on output-sink state exclusively held by the
`Interpreter` and are unreachable from the VM (native IO Tier-2 made only File native; Stdout/Stderr remained a
fall-through). To make it VM-native, the VM needs access to the **output-sink state**.

## Map of the output-sink state (investigation finalized 2026-06-11)

`Interpreter` state that `emit_output` (`mod.rs:4229`) depends on:

| Field | Role | per-thread? |
|-----------|------|------------|
| `output: String` (`mod.rs:828`) | Output accumulator when not immediate. `run()` returns `self.output.clone()` (`run.rs:865`). Consumed via `take_output`/`output()`/`clear_output` | ○ a thread clone `mem::take`s it into the promise (`builtins_system.rs:155`) |
| `stderr_output: String` (`829`) | stderr buffer (same as above) | ○ same |
| `output_emitted: bool` (`846`) | Flag: has anything been output. `has_output_emitted()` | ○ |
| `immediate_stdout: bool` (`843`) | **Whether to write immediately to the real stdout**. **CLI execution is true** (`main.rs:466`) → emit_output writes directly with `std::io::stdout().write_all`. embedded/EVAL/test are false → buffer | thread clones are true (`socket_thread.rs:386`) |
| `is_thread_clone` + `shared_thread_output: Option<Arc<Mutex<String>>>` (`1131`/`1135`) | Shared buffer so thread clones interleave output in real chronological order | clone-specific |
| `tap.subtest_depth()` (`tap_state.rs:130`) | **Suppresses immediate writes during a subtest** (only `subtest_depth()==0 && immediate_stdout` reaches the real stdout) | tap is also Interpreter-owned |

The decision tree of `emit_output` itself:
```
output_emitted = true; (add to the Stdout handle's bytes_written)
if tap.subtest_depth()==0 && immediate_stdout: write_all+flush to the real stdout
elif is_thread_clone && shared_thread_output: push to the shared buffer
else: push to self.output
```

`emit_output` is called from **37 sites** (subtest/test_functions/io/supply/handle/builtins_system/vm_hyper_race_parallel/
main_args …). `output` reads/writes number **~33 sites**.

## ★What is decisively different from io_handles (the strategic crux)

1. **The output sink is "single-owner, consumed at the end"**. io_handles used a per-thread snapshot + merge-back,
   but `output` is the program output itself, `clone()`d and returned by `run()` at the end. A thread clone hands it
   to the promise via `mem::take`, concatenated at await. = per-thread, but "collected by the promise" rather than
   "merge-back".
2. **The dependencies chain**: emit_output's **write decision** depends not only on `immediate_stdout` (a simple bool)
   but also on **`tap.subtest_depth()`** (the TAP state machine). For the VM to make Stdout output native, the TAP
   subtest depth must also be VM-reachable → **the transfer/referencing of tap gets dragged in**. This is why "the
   main keep is heavy".
3. **CLI execution is immediate** (writes directly to the real stdout, bypassing the buffer). So "just sharing the
   buffer" does not make the CLI path native — the **entire write decision (immediate + subtest + thread)** must be
   factored out into the OutputSink.

## Strategy: OutputSink abstraction + shared handle (the io_handles playbook)

Factor the output sink's **state + write decision** into a single `OutputSink` struct, shared between VM↔Interpreter
as `Arc<RwLock<OutputSink>>` (same shape as io_handles; reuse the generic guard in `lock_reentry.rs`).

```rust
// src/runtime/output_sink.rs
pub(crate) struct OutputSink {
    pub(crate) output: String,
    pub(crate) stderr_output: String,
    pub(crate) output_emitted: bool,
    pub(crate) immediate_stdout: bool,
    pub(crate) is_thread_clone: bool,
    pub(crate) shared_thread_output: Option<Arc<Mutex<String>>>,
}
impl OutputSink {
    // subtest_active is passed by the caller (the Interpreter owning tap / the future VM).
    // Before the TAP transfer, we only take a bool for "inside a subtest or not" (tap as a whole does not move).
    pub(crate) fn emit(&mut self, text: &str, subtest_active: bool) { ... }
    pub(crate) fn emit_stderr(&mut self, text: &str, subtest_active: bool) { ... }
}
// Interpreter.output_sink: Arc<RwLock<OutputSink>>
```

Handling the TAP dependency: **pass "inside a subtest or not" as a bool to OutputSink::emit** (the whole TAP state
machine does not move). The Interpreter computes and passes `self.tap.subtest_depth()==0`. For VM-native Stdout
dispatch the VM also needs to know the subtest depth, but that is handled by a separate small slice passing a tap
reference to the VM (or avoided via "fall through if inside a subtest at the moment the VM outputs to Stdout").

## Slice plan (strangler-fig; each PR behavior-invariant; CI is the safety net)

- **PR-A = extract the `OutputSink` struct (still a plain field)**: consolidate `output`/`stderr_output`/`output_emitted`/`immediate_stdout`/
  `is_thread_clone`/`shared_thread_output` into `Interpreter.output_sink: OutputSink` (**still a plain field, not yet
  `Arc<RwLock>`**). Move `emit_output`/`emit_stderr` into `OutputSink::emit(text, subtest_active)`, with the
  Interpreter wrapper computing `self.tap.subtest_depth()==0` and delegating (one operation, one implementation).
  The 37 callers and ~33 accesses go through accessors. Behavior-invariant.
- **PR-B = lift `output_sink` to `Arc<RwLock<OutputSink>>`**: same shape as io_handles PR-B. Reuse the generic guard
  in `lock_reentry.rs`, `output_sink()`/`output_sink_mut()` accessors, `clone_for_thread` gets a fresh per-thread sink
  (threads swap the whole sink or snapshot instead of mem::take), promise collection goes via the sink. Behavior-invariant.
- **PR-C = transfer the output_sink handle to the VM + first VM-native Stdout/Stderr dispatch**: same shape as
  io_handles PR-C. Make the **Stdout/Stderr receivers** of `$fh.say/print/put` native using the VM's `output_sink`.
  Payloads use the existing render_* (same as Tier-2a). Fall through while inside a subtest (or a small trick passing
  the subtest depth to the VM). This eliminates native IO Tier-2's "Stdout/Stderr fall through".
- **PR-D+ = making the read side (get/lines/read/slurp + ArgFiles `@*ARGS` env / non-UTF8 decode) VM-reachable**. This
  depends on different state (the env's `@*ARGS`, encode/decode), hence a separate slice.
- **Final fold (④/⑤)**: once the tree-walk execution path is gone, fold output_sink / io_handles into plain VM fields.

## Discipline (identical to io_handles)

Never **re-acquire the RwLock guard re-entrantly on the same thread** (detected by the debug guard). emit_output is
called from 37 sites, some of which run after/before render_* (method-dispatch re-entry), so **never call render_*
across the guard** (build payloads outside the guard). The last line of defense is make roast.

## Scope / non-goals

- Scope = factor the output-sink state into OutputSink, make it a shared handle, make Stdout/Stderr output dispatch
  VM-native, and eliminate the Stdout/Stderr fall-through of native IO Tier-2. Each PR behavior-invariant.
- Non-goals: transferring the whole TAP state machine (we stop at passing subtest_active as a bool), transferring the
  read-side decode/ArgFiles (PR-D+), folding output_sink into a plain VM field (④/⑤).

## Progress

- **Design finalized (this document, 2026-06-11)**: mapped the output-sink state (output/stderr_output/output_emitted/immediate_stdout/
  thread-clone/tap subtest_depth, emit_output's 37 callers). Adopted the io_handles playbook (extract OutputSink →
  Arc<RwLock> → VM handle → native dispatch). Crux = the chained TAP subtest dependency (avoided by passing
  subtest_active as a bool).
- **PR-A completed (2026-06-11)**: consolidated 7 fields
  (output/stderr_output/output_emitted/immediate_stdout/is_thread_clone/shared_thread_output/shared_thread_stderr)
  into the `OutputSink` struct (`src/runtime/output_sink.rs`), replacing the `Interpreter`'s scattered fields with a
  single `output_sink: OutputSink` (**still a plain field**). Moved `emit_output`'s write decision into
  `OutputSink::emit(text, subtest_active)`, with the Interpreter wrapper computing `tap.subtest_depth()!=0` and
  delegating (the Stdout handle's `bytes_written` accounting stays in the wrapper since it depends on io_handles).
  `clone_for_thread` builds a thread OutputSink (Arc-cloning the shared buffer from the parent). ~85 access sites
  (self/thread_interp etc.) now go through `.output_sink.FIELD` (same shape as the `TapState` extraction). Behavior
  fully invariant (stdout/stderr/thread/subtest/EVAL confirmed to match raku; `make test` cargo 461 green).
  Next = **PR-B** (lift `output_sink` to `Arc<RwLock<OutputSink>>`, same shape as io_handles PR-B, reuse `lock_reentry.rs`).
- **PR-B completed (2026-06-11)**: `output_sink: OutputSink` → `Arc<RwLock<OutputSink>>` (same shape as io_handles PR-B).
  `OutputSinkReadGuard`/`OutputSinkWriteGuard` type aliases (reusing the generic guard in `lock_reentry.rs`) +
  `output_sink()`/`output_sink_mut()` accessors. ~85 sites go through the guards (read/write is **compiler-enforced** =
  a wrong write is caught as E0596/E0594). `output()` can no longer return `&str` across the guard and now returns
  `String` (clone). `clone_for_thread` builds the thread's OutputSink as an `Arc<RwLock>`.
  **Crux: Rust 2021 if-let temporary lifetimes** — in `if let ... = self.output_sink()... { ...
  self.output_sink_mut()/emit_output ... }` the read guard stays alive across the body, producing a **runtime reentry
  panic** on write-while-read (it compiles, because the accessor takes `&self`). Restructured the 5 thread/supply
  drain sites into "clone the Arc out with a `let` → drop the guard at the `;` → body". Behavior-invariant
  (threads/supply/subtest/warn/EVAL confirmed with zero reentry panics, cargo 461 green).
  Next = **PR-C** (transfer the output_sink handle to the VM + VM-native Stdout/Stderr dispatch).
- **PR-C completed (2026-06-11)**: output_sink handle transferred to the VM + **VM-native Stdout/Stderr output dispatch**.
  This removes the "Stdout/Stderr fall through" remaining from native IO Tier-2; **all targets
  (File/Stdout/Stderr) of `$fh.say/print/put/printf/print-nl` are now VM-native**.
  - `Interpreter::output_sink_handle()` (Arc clone) + VM `output_sink` field/`output_sink_mut()` accessor (same shape as io_handles PR-C).
    `Interpreter::subtest_active()` (`tap.subtest_depth()!=0`; TAP stays interpreter-owned, passed to the VM as a bool).
  - New `OutputSink::emit_stderr(text, subtest_active)` (unifies the Stderr branch into 1 impl; the interpreter's
    Stderr branch delegates). `IoHandleState` gains `is_stdout_target`/`is_stderr_target`/`add_bytes_written`, and
    `native_text_write` is split into `prepare_text_payload` (closed+nl_out+bytes accounting, target-independent) and
    the write. `native_print_nl` removed (print-nl unified as content="" + newline=true, trying="write").
  - VM `vm_emit_stdout` (scan-bump of the Stdout handle's bytes_written = same shape as emit_output + sink.emit) /
    `vm_emit_stderr` (sink.emit_stderr). `try_native_io_handle_output` extended into 3 branches File/Stdout/Stderr
    (target determined before payload construction; Socket/non-UTF8 File/Stdin fall through). Stdout has **double
    bytes_written accounting** (the receiver in prepare + the scan in emit) = **identical to main's existing
    behavior** (raku counts once = pre-existing difference, out of scope). Inside subtests sink.emit still buffers
    correctly via subtest_active.
  - **Verification**: new `t/io-handle-stdout-stderr-native.t` (6; is_run captures the subprocess's real stdout/stderr)
    mutsu PASS, expected values match raku. `$*OUT/$*ERR` output is **0% method-fallback**. subtest indent / tell
    double accounting are **identical to main** (confirmed pre-PR-C) = pre-existing, unchanged.
    `make test` green. Next = **PR-D+** (making the read side get/lines/read/slurp + ArgFiles `@*ARGS`/non-UTF8 decode VM-reachable).
- **PR-D (read side, first slice `get`) completed (2026-06-11)**: made `.get` (single line read) VM-native for File+UTF8 targets.
  **Investigation finalized: the read side also splits by destination** — the **File+UTF8/bin branch of
  `read_line_from_handle_value` is pure handle state** (`read_record_with_separators(file, seps, chomp)`, UTF-8 lossy,
  no `self` re-entry). **ArgFiles** (depends on the `@*ARGS` env) / **Stdin** (`std::io::stdin`) / **non-UTF8 File**
  (`decode_with_encoding` re-entry) / **Socket** fall through.
  - Made `read_record_bytes`/`read_record_with_separators` (pure, static) `pub(crate)` (1 impl of record reading).
    Added `IoHandleState::read_line_native()` (closed check + seps/chomp + `read_record_with_separators`).
  - VM `try_native_io_handle_read` (early gate, right after byte_output, both mut/non-mut paths): native only for
    `get`, no args, **File+UTF8 (`can_native_text_write`)**; everything else falls through. The result is shaped as
    `Str`/`Nil` just like the interpreter's `get`.
  - **Verification**: new `t/io-handle-get-native.t` (10) — 10/10 on both mutsu and raku (chomp/EOF→Nil/:!chomp/custom
    nl-in/latin1 fall-through/closed dies). `MUTSU_VM_STATS` shows File+UTF8 get native (latin1 get falls through with
    get=1). io-handle.t not-ok count identical to main (24 = pre-existing). `make test` green.
  - **Next = PR-D2+**: same pattern to make `lines` (lazy Seq)/`words`/`slurp` (read all→String/Buf)/`read` (N bytes→Buf)/`getc`/`readchars`
    native for File+UTF8. **True elimination of ArgFiles/Stdin/non-UTF8 decode requires making `@*ARGS` (env) /
    `decode_with_encoding` VM-reachable** (env transfer = separate campaign, entangled with tracks A/B/C).
- **PR-D2 (read side, bulk read `slurp`/`read`) completed (2026-06-11)**: made `.slurp` (→Str) and `.read` (→Buf) native for File+UTF8.
  - Added to `IoHandleState`: `can_native_slurp_string(has_bin_arg)` (gate: File+UTF8, not :bin, not bin-mode) /
    `slurp_string_native` (`read_to_end`+UTF-8 lossy) / `read_bytes_native(count)` (count>0 = one read up to count /
    count=0 = read all; encoding-independent). Made `make_buf` (Buf construction) `pub(crate)`.
  - Extended VM `try_native_io_handle_read` to the 3 methods get/slurp/read. **slurp** falls through for :bin/non-UTF8/bin-mode
    (Buf/decode stays in the interpreter); **read** is File-only (raw bytes = encoding-independent), going through
    `Interpreter::make_buf`. Junctions fall through.
  - **Verification**: new `t/io-handle-slurp-read-native.t` (10) — 10/10 on both mutsu and raku (slurp whole/after a
    partial read/EOF empty, read N→Buf/EOF empty, :bin→Buf fall-through, latin1 decode fall-through, closed dies).
    `MUTSU_VM_STATS` shows String slurp/read native (:bin slurp falls through with slurp=1). io-handle.t not-ok=24
    (pre-existing). `make test` green.
  - **lines/words are lazy (`LazyIoLines`) = even making method dispatch native, the actual read happens at iterator
    consumption; only the value assembly would be native = little benefit**. `getc`/`readchars` (char reads) are the
    next slice candidates. The remaining ArgFiles/Stdin/non-UTF8 still require the env/decode transfer (unchanged).
- **PR-D3 (read side, char read `getc`/`readchars`) completed (2026-06-11)**: made `.getc` (→Str/Nil)/`.readchars` (→Str) native for File+UTF8.
  - Made `read_utf8_char` (pure, static, reads 1 UTF-8 char) `pub(crate)`. Added
    `IoHandleState::read_chars_native(count: Option<usize>)` (count=Some(n) = n-char loop / None = read_to_end+UTF-8 lossy).
  - Added getc/readchars to VM `try_native_io_handle_read`. The gate is `can_native_text_write` (File+utf8/bin) =
    **utf16 is excluded** (needs special BOM/endian handling, so it falls through to the interpreter); non-UTF8/Stdin
    also fall through. getc maps empty→Nil; readchars parses count with `parse_out_buffer_size` just like the
    interpreter (invalid → same error).
  - **Verification**: new `t/io-handle-getc-readchars-native.t` (10) — 10/10 on both mutsu and raku (getc multibyte
    é/EOF→Nil, readchars N/EOF empty/no-arg read-all, latin1 fall-through). `MUTSU_VM_STATS` shows getc/readchars
    native (latin1 falls through). **utf16.t green** = utf16 getc/readchars correctly fall through. io-handle.t
    not-ok=24 (pre-existing). `make test` green.
  - **Where read-side nativization stands**: single-handle reads `get`/`slurp`/`read`/`getc`/`readchars` are all native
    for File+UTF8. Remaining: **lines/words (lazy, little benefit)** and **ArgFiles/Stdin/non-UTF8 decode (= requires
    making the `@*ARGS` env / `decode_with_encoding` VM-reachable; env transfer = separate campaign)**. One-shot IO
    output and reads are essentially eliminated.
