# ADR-0112: A call-only `my sub` in a routine body is a frame lexical, not a registry entry

- **Status**: Accepted (2026-09-23); first slice implemented in the same PR as the decision
  (see "Implementation status").
- **Context**: [#9103](https://github.com/tokuhirom/mutsu/issues/9103) (follow-up to #9081,
  #9073, parent #8673).

## 1. Problem

A `my sub` declared in a routine body is lexical to that body. mutsu implemented the
lexical scope on top of the one program-global routine registry:

1. every call of the enclosing routine executed the declaration's `RegisterDecl` twice
   (the hoisted registration and the in-sequence one), each installing the inner sub into
   the registry and invalidating the name-keyed resolution caches (`fn_resolve_gen`);
2. the call snapshotted the whole routine registry on entry and restored it on return, so
   the next call started over;
3. the declaration set `CompiledFunction::has_inner_subs`, which bars the enclosing routine
   from the positional-light, light, OTF-cache and TRIR call paths;
4. each call to the inner sub resolved its name through the registry, whose generation had
   just moved, so none of the generation-tagged memos helped.

On a microbenchmark (a two-line routine declaring a one-line inner sub that it calls twice)
the enclosing routine cost 71.4K instructions per call against 12.7K for the same code with
the inner sub hoisted to file scope. `RegisterDecl` alone was 40% of the program. The same
shape is JSON::Fast's `unjsonify-string` / `fetch-codepoint`.

## 2. Decision

When the enclosing routine body provably does **nothing with the inner sub except call it
by its bare name**, the inner sub is bound as a *frame lexical*:

- **It never enters the registry.** Its `RegisterDecl` still runs, but it only derives the
  routine's definition — once per interpreter, through the ordinary registration inside a
  registry snapshot/restore, so every check and signature normalization a declaration
  owes is still performed — and memoizes it in
  `Interpreter::frame_lexical_routines`. Every later execution is one table probe.
- **Its call sites are resolved at compile time.** Each chunk that calls it lists it in
  `CompiledCode::lexical_routines`. The `CallFunc`, `CallFuncNamed`, `ExecCall` and
  `ExecCallPairs` handlers consult that table before any name-keyed resolution or cache,
  and dispatch straight to the routine's compiled body through the fast, positional-light,
  light or named call path, whichever the callee is eligible for. The opcodes themselves
  are unchanged, so the JIT needs no change either.
- **The enclosing routine keeps its light call paths.** A frame-lexical declaration no
  longer counts toward `has_inner_subs` or `declares_inner_routines`, so the body takes the
  positional-light path and no registry snapshot is taken. The `RegisterDecl` op stays in
  the body, which keeps every local of the enclosing frame env-synced
  (`compute_needs_env_sync`'s "defines a lazy body" rule) — the inner sub still reads the
  enclosing routine's lexicals by name, exactly as before.
- **The memo key is compile-time data**: the routine's name, the package its compiled
  body was keyed under, and that body's fingerprint (`FrameLexicalRef`). The declaration and
  every call site carry the same triple, so they agree without consulting any registry, and
  compiled-function key renames during import cannot desynchronize them.

### What "provably only called" means

Decided by `compiler/frame_lexical_routines.rs` after the routine body is compiled:

- The declaration itself is plain: not `multi`, no computed name, no traits, not
  exported, no `is rw`/`is raw`, no alternate signatures, not the body's final statement
  (whose value is the `&name` code object), a plain identifier that no builtin answers to,
  declared once in the body, parameters carrying only `copy`/`rw`/`raw`/`readonly`.
- The body's AST, serialized, mentions the name only as the callee of a `Call` node and as
  the declaration. Any other mention — `&name`, a qualified `Pkg::name`, a string literal —
  disqualifies that name. Any `EVAL`, symbolic or indirect lookup, pseudo-package
  (`MY::`, `OUTER::`, `CALLER::`, ...), `callframe`, `&?ROUTINE`, dispatcher redispatch
  (`samewith`, `callsame`, ...) or lexical type declaration disqualifies every name in the
  body.
- The compiled bytecode agrees: every op that names the routine is a bare call in the
  body, one of its closures, or a routine declared in it, and no body stashed in a
  `stmt_pool` for run-time compilation mentions it.
- The routine has no `state`/`once` storage and is not `is cached`: that storage is keyed by
  the registration clone the routine no longer gets.

Anything outside the proof keeps the registry-based behaviour unchanged.

## 3. Rejected alternatives

- **Keep registering, make the churn cheaper.** Every layer of the per-call install is
  already memoized (`prepared_fn_defs`, the idempotent re-registration path, the per-key
  invalidation of #8314). What remained was structural: a lexical scope implemented as a
  global table that is written and rolled back on every call.
- **Bind `&name` in the frame's env and resolve bare calls through it.** A `&name` value
  created at the hoisted declaration captures the env of the body's entry, before the body's
  own `my` declarations run, so a first-class `&name` that escapes would read stale
  captures. The call-only restriction removes the need for a code object at all; a
  first-class `&name` keeps the registry path until a later slice gives it one.
- **A new opcode for lexical calls.** It would have to be taught to the JIT and duplicate
  the argument marshalling of four call opcodes; a per-chunk table consulted by the
  existing handlers needs neither.

## 4. Consequences

- The name-keyed dispatch caches can no longer be poisoned by an inner sub: a frame-lexical
  call never reads or fills them, so a same-named package routine on either side of the
  call is unaffected.
- An inner sub that is used as a value (`&name`, returned, passed to `.map`) still takes the
  registry path. Extending the frame-lexical binding to those uses needs a code object whose
  captured env is taken at the point of use, and is the natural next slice.
- Known and unchanged: a closure that escapes its routine and then calls an inner sub reads
  the inner sub's free variables from the caller's env, not from the routine's frame. That
  is a pre-existing gap of the dynamic free-variable resolution of named inner subs; it is
  neither introduced nor fixed here.

## Implementation status

- Slice 1 (this ADR's PR): call-only inner subs of `sub`/`method` bodies compiled through
  `Compiler::compile_sub_body`.
