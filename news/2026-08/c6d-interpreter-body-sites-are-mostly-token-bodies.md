# C6d closed out: 83% of its sites were token bodies, and that turned out to be a structural guarantee

ADR-0019's C6d box ("every interpreter execution site that ran a routine or code object's AST
body must run its compiled bytecode instead") started from a wrong premise, which this memo was
opened to correct: the checklist described the remaining carriers as "reached precisely *because*
an OTF gate rejected the routine, so eliminating them means widening OTF coverage to every
routine". A survey said otherwise, the box was subdivided against the survey instead of against
the premise, and every resulting slice has now landed. This file records the measurement, the
slices, and the one finding that outlived them.

## The original measurement

Six `&def.body` execution sites were instrumented with one env-gated `eprintln!` printing the site
and the routine name, and the whole `t/` suite was run once with the probe on
(`MUTSU_C6D_SURVEY=1 prove -j4 -e target/debug/mutsu -r t/`). 1148 hits:

| site | hits | what it is |
| --- | --- | --- |
| `dispatch.rs:eval_token_def` | 632 | grammar `token`/`rule` body |
| `regex_token_resolve.rs:resolve_token_patterns_with_args_in_pkg` | 324 | grammar `token`/`rule` body |
| `calls.rs:call_function_def` | 144 | ordinary routine |
| `calls.rs:exec_call` | 48 | ordinary routine |
| `dispatch_proto_call.rs:call_proto_dispatch` | 0 | proto dispatch |
| `types/roles.rs:run_role_submethod` | 0 | role submethod |

956 of 1148 hits (83%) were grammar token/rule bodies — top names the grammar rules themselves
(`TOP` 483, `expr` 199, `block` 29, `block-string` 21). The ordinary-routine tail was 192 hits over
~37 distinct names, dominated by multi dispatch through `where` constraints with `nextsame` (all
102 `seq` hits came from `t/multi-where-otf-dispatch.t`'s `proto sub seq($) {*}` plus three
`where`-constrained candidates that `nextsame` through each other), the rest being the
callsame/callwith/nextsame/nextwith family, user operators (`postfix:<!>`, `infix:<op>`), and
`Test::Util`-style helper subs. None of it matched the four constructs the OTF-gate doc comment
listed (`ClassDecl`/`RoleDecl`, `start`, cross-thread `state`, sigilless-scalar param + `EVAL`) —
a routine whose body declares a class does not reach these sites at all.

A second correction the survey produced: these sites did **not** tree-walk. `run_block` →
`run_block_raw` → `compile_block_raw(stmts)` + `run_nested` compiled the body to bytecode on every
call, so the cost C6d removed was a per-call recompile, not interpretation — a smaller correctness
risk and a clearer win than the checklist implied.

The six-site inventory was itself incomplete, because the original grep matched
`run_block(&def.body)` / `eval_block_value(&def.body)` but not the other execution forms. A fresh
sweep for all `&def.body` / `&data.body` executors found
`builtins_operators_fallback.rs:call_function_fallback`'s def arm
(`eval_block_value_with_pre_post(&def.body)`) at **410 hits** — the largest live ordinary-routine
site of the whole campaign, missed entirely by the first pass — and
`methods_mut_proxy.rs:call_proxy_callback`'s `run_block(&data.body)` at 2 hits, both anonymous
blocks and therefore block-family, out of C6d scope.

## What landed

- **C6d-1 — the ordinary-routine tail.** Following `call_function_def`'s callers showed this was
  not a calling-convention problem at all: `compile_and_call_function_def`, the VM's routine entry,
  already existed and most callers used it; the tail was the handful that still reached the
  *interpreter* entry. Each was rewired, not redesigned. The one non-obvious constraint, found by
  trying the naive rewire, is that the multi-deferral caller must **not** use
  `compile_and_call_function_def` — that entry pushes a fresh multi-dispatch frame for the name,
  and a deferral chain owns the frame it just advanced, so the chain defers to the same candidate
  forever and the stack overflows. It uses the entry below that setup,
  `call_compiled_function_named`
  (`news/2026-08/multi-deferral-runs-the-compiled-candidate.md`). The remaining five callers
  (`builtins_operators_fallback`, `builtins_operators_infix`, `builtins_operators_coerce`,
  `accessors_state`, `main_args`) went through one shared `call_routine_def`
  (`news/2026-08/user-operators-run-their-compiled-body.md`), and `exec_call`'s inlined copy of the
  whole `call_function_def` body was folded in #5946. `call_function_def` itself is deleted.
- **C6d-3.** `call_proto_dispatch` was not left as a coverage argument: its proto-sub arm got the
  same `call_routine_def` fold plus its first pin, `t/proto-dispatch-interpreter-path.t` (#5947).
  `run_role_submethod`'s `def` is a `MethodDef`, not a `FunctionDef`, so it moved to Phase D, where
  D8-3 put it on its precompiled chunk (it now runs `run_compiled_block_raw` and keeps the raw-AST
  carrier only for a method installed by a meta-programming hook).
- **C6d-4.** The code-object path (`call_sub_value`) landed as #5948, with an rw gate documented in
  `todo/tickets/rw-writeback-through-wrap-chain-needs-shared-cells.md`.
- **C6d-5.** `call_function_fallback`'s 410-hit def arm was folded in #5950, gated on
  `def_module_single_sig_body_ok_ignoring_state`; the gate-rejected shapes (sigilless scalar param
  with EVAL-boundary writeback, interpreter-coupled bodies) keep the interpreter arm on purpose.

A related discovery closed the last reason `call_function_def` had to exist: multi candidates used
to arrive with `compiled: None`, because `vm_register_sub_ops` had `if *multi { continue; }` exactly
where it attached a compiled routine key. Registration now installs each candidate from the routine
its plan names, and `multi_candidate_state_forces_interpreter`, `call_function_def`, and the flag
that drove them are all deleted — `news/2026-08/multi-candidates-run-their-plan-compiled-body.md`.

## C6d-2: not deferred work, a structural guarantee

The 83% majority — the two token/rule sites — was tracked as C6d-2, "grammar token/rule bodies stay
interpreter-executed; that is ADR-0009's execution model, not this box's. Tracked until the
token/rule work lands, then closed together with F7." F7 landed both its slices on 2026-08-17
(top-level `token`/`rule` declarations onto `CompiledTokenDeclPlan`, then class-body ones onto
`ClassBodyOp::TokenRule`), so that closing condition is met and C6d-2 is closed with it.

Re-reading the two sites at close-out sharpened *why* they can never be an OTF-coverage question,
beyond "ADR-0009 says so". A token/rule body is not a routine body that happens to be rejected by a
gate — it is **structurally guaranteed** to be a single regex literal:

- there is exactly one construction site for `Stmt::TokenDecl`/`Stmt::RuleDecl` in the whole
  codebase, `src/parser/stmt/class/grammar_module.rs`, and it always builds
  `vec![Stmt::Expr(Expr::Literal(Value::regex(pattern)))]`;
- `register_token_decl` (`registration_sub.rs`), the sole feeder of `insert_token_def`, always sets
  `compiled: None`, exactly as ADR-0009 decided;
- an AST dump confirms it — `token TOP { <word>+ % \s+ }` lowers to one
  `Expr(Literal(Regex(":ratchet <word>+ % \\s+ ")))` statement.

So `eval_block_value(&def.body)` at `eval_token_def` and
`resolve_token_patterns_with_args_in_pkg` is a **constant fetch**, not a tree-walk of a routine
body. The work those two sites really do is around the call, not in it: parameter binding,
`interpolate_bound_regex_scalars`, `bake_bound_params_into_regex_code_blocks`, `<sym>`
instantiation, and (in the resolver's case) a scratch `Interpreter`. Any future cost work there is
a regex-execution question scoped against ADR-0009, and nothing about it is C6d-shaped.

## Verification at close-out

Both sites were confirmed still live rather than assumed — a `rust-gdb -batch` breakpoint on
`dispatch.rs`'s `eval_block_value(&def.body)` fires on a plain `G.parse(...)`. A comparison script
covering the constructs the survey named — `where`-constrained multi with `nextsame`/`callsame`,
user `postfix:<!>` and `infix:<op>`, a parameterized grammar subrule (`token chunk($n) { \w ** {$n} }`),
and a `proto token` with `multi token :sym<>` candidates under a separated-quantifier `rule` — is
byte-identical between `raku` and `mutsu`, exit code included.

ADR-0019's C6d-2 checkbox is checked as part of this close-out, which also makes the ADR's own
status paragraph ("Phases A, B, and C are fully closed") self-consistent — it was the last
unchecked box anywhere in Phase C.
