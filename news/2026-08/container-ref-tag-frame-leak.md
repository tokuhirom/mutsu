# `TagContainerRef` no longer leaks across call frames — Text::CSV t/90_csv.t passes 524/524

Closes the 90_csv frontier: after #6342's closure-writeback and `.VAR.name`
work let the file run all 524 tests, the last two failures (507 "AOH parse out
with kh defaults to Hash" and 508 "Headers kept") were ticketed as "a named
`@`-array argument loses container identity, only in a large file". The
diagnosis in that ticket was a red herring at the mechanism level — the named
argument was never copied in the call chain. What actually happened, found
with a hardware watchpoint on the corrupted slot:

1. `TagContainerRef` (the "this topic/loop source came from container X, at
   baked local slot N" signal for for/given container writeback) stores into
   `container_ref_var`, an **interpreter-wide field**, and is designed to be
   consumed by the very next for/given op **in the same code object**.
2. Text::CSV's `method CSV` executes such a tag for its own `my @in`
   (slot 28 of the *method* frame) on a path where no same-frame consumer
   runs, so the tag survives the method return.
3. The test script's next `for in () -> $in { ... }` loop iterates a **sub
   call** — which emits no tag of its own — so `exec_for_loop_body` adopted
   the stale `("@in", slot 28)` tag and ran per-iteration container
   writeback against it: resolving the name in the *caller's* env (the
   script's own same-named `my @in`!) and writing the rebuilt array into the
   *caller's* locals[28], which in t/90_csv.t was the `__do_decl_init` marker
   slot of the second `kh => my @kh` bare declaration.
4. A poisoned (non-Nil) init marker makes the bare-decl expression skip its
   declaration entirely and read `@kh` back by name — autovivifying a fresh,
   descriptor-nameless array. Text::CSV's rakudo#2483 gate
   (`@kh.VAR.name ne "element"`) then saw "element", never defaulted
   `out => Hash`, and returned AoA with an empty `@kh`.

The "only in a large file" mystery dissolves: the corruption needs (a) a
callee that leaves an unconsumed tag, (b) a caller loop over an untagged
(call-expression) source, (c) a same-named caller variable so the writeback's
env read finds an array, and (d) an array-or-Nil value at the stale slot
index — the 337-line file satisfied all four, every hand reduction missed one.

Fix: `container_ref_var` now carries the fingerprint of the `CompiledCode`
that set it (`resume_code_fp`, the same identity used by `resume_ip`), and
every consumer goes through `take_container_ref_for(code)`, which discards a
tag whose fingerprint does not match the consuming frame's code. The signal
is one-shot and same-frame by construction, so a mismatch is always a leaked
callee tag.

Pinned by `t/container-ref-tag-frame-leak.t` (fails on the pre-fix binary:
the caller's same-named array is clobbered with the loop items; the minimal
shape needs an implicit-topic loop and array canaries so the writeback
doesn't bail early). With the fix, Text::CSV's `t/90_csv.t` passes
**524/524** (from 494/496-with-abort at the start of the campaign session,
then 522/524 after #6342). Next CSV frontier: `91_csv_cb.t` /
`92_csv_encoding.t`.
