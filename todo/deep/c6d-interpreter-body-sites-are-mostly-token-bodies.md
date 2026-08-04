# C6d is not "widen OTF coverage to every routine": 83% of its sites are token bodies

Scoping measurement for ADR-0019 C6d, which the checklist described as "these carriers
are reached precisely *because* an OTF gate rejected the routine, so eliminating them
means widening OTF coverage to every routine". The measurement says otherwise, so the
next pass should not start from that premise.

## Method

There are six `&def.body` execution sites. Each was instrumented with one env-gated
`eprintln!` printing the site and the routine name, and the whole `t/` suite (2881
files) was run once with the probe on:

```
MUTSU_C6D_SURVEY=1 prove -j4 -e target/debug/mutsu -r t/
```

1148 hits. (`t/vendored-real-test-module.t` fails under the probe because it compares
stderr; that is the probe's artifact, not a regression.)

## Result

| site | hits | what it is |
| --- | --- | --- |
| `dispatch.rs:eval_token_def` | 632 | grammar `token`/`rule` body |
| `regex_token_resolve.rs:resolve_token_patterns_with_args_in_pkg` | 324 | grammar `token`/`rule` body |
| `calls.rs:call_function_def` | 144 | ordinary routine |
| `calls.rs:exec_call` | 48 | ordinary routine |
| `dispatch_proto_call.rs:call_proto_dispatch` | 0 | proto dispatch |
| `types/roles.rs:run_role_submethod` | 0 | role submethod |

Three things follow.

**1. 956 of 1148 hits (83%) are grammar token/rule bodies.** Those `FunctionDef`s hold a
*regex* body, not a routine body the compiler would compile as a routine, and their
execution model is ADR-0009's, not the OTF gate's. Top names are the grammar rules
themselves — `TOP` (483), `expr` (199), `block` (29), `block-string` (21). Whatever C6d
does about them is a regex-execution question and should be scoped against ADR-0009.

**2. The ordinary-routine tail is small and specific.** 192 hits over ~37 distinct
names, and it is dominated by *multi dispatch through `where` constraints with
`nextsame`*: all 102 `seq` hits come from `t/multi-where-otf-dispatch.t`'s
`proto sub seq($) {*}` + three `where`-constrained candidates that `nextsame` through
each other. The rest of the tail is the same family (`cs`, `cw`, `ns-multi`,
`cw-multi`, `cs-multi`, `nw-multi` — the callsame/callwith/nextsame/nextwith tests),
user operators (`postfix:<!>`, `infix:<op>`), and `Test::Util`-style helper subs
(`is_run`, `group-of`, `tap-ok`, `is-deeply-junction`, `assert-eq`). None of that
matches the four constructs the OTF-gate doc comment lists (`ClassDecl`/`RoleDecl`,
`start`, cross-thread `state`, sigilless-scalar param + `EVAL`) — a routine whose body
declares a class does *not* reach these sites (`MUTSU_VM_STATS` reports 0 function
fallbacks for `sub f($x) { class C { has $.a }; C.new(a=>$x).a }`).

**3. Two of the six sites are dead in the whole suite, and one of them is not C6d's.**
`run_role_submethod`'s `def` is a `MethodDef`, not a `FunctionDef`, so it belongs to
Phase D's class/role work. `call_proto_dispatch` needs only a coverage argument.

## The convention question C6d actually has

These sites do **not** tree-walk. `run_block` → `run_block_raw` →
`compile_block_raw(stmts)` + `run_nested`, so the body is **compiled to bytecode at
every call**. The cost C6d removes is therefore a per-call recompile, not
interpretation — a smaller correctness risk and a clearer win than the checklist
implies.

But `compile_block_raw` compiles the body as a *block*: the caller has already bound
the arguments (`bind_function_args_values` just above the call in
`call_function_def`). `def.compiled` is routine bytecode that binds its own parameters
from `param_local_slots`. So handing `def.compiled` to these sites double-binds, which
is a real convention mismatch — a *different* one from the one C6c turned out not to
have. The two candidate shapes are: compile the body as a block once and memoize that
chunk on the def beside `compiled`, or lift these sites to call the routine convention
and delete their own argument binding.

## Update: C6d-1's shape turned out to be neither candidate

Following the `call_function_def` callers showed the ordinary-routine tail is not a
convention problem at all. `compile_and_call_function_def` — the VM's routine entry —
already exists and most callers use it; the tail is the handful of callers that still
reach the *interpreter* entry `call_function_def`:
`builtins_dispatch_next` (multi deferral), `builtins_operators_fallback` (user
operators), `builtins_operators_infix` (reduce), `builtins_operators_coerce`,
`accessors_state`, `main_args` (`MAIN`). Each is rewired, not redesigned.

The one non-obvious constraint, found by trying the naive rewire: the multi-deferral
caller must **not** use `compile_and_call_function_def`, because that entry pushes a
fresh multi-dispatch frame for the name and a deferral chain owns the frame it just
advanced — the chain then defers to the same candidate forever and the stack overflows.
It uses the entry below that setup, `call_compiled_function_named`. Landed; see
`news/2026-08/multi-deferral-runs-the-compiled-candidate.md`.

## Update 2: the remaining callers landed, and re-measuring shrank the site

All five remaining callers now go through one `call_routine_def`
(`news/2026-08/user-operators-run-their-compiled-body.md`). Re-running the probe on the
whole `t/` suite *after* the deferral slice found only **13** hits left at
`calls.rs:call_function_def`, not the ~42 the arithmetic above implies: 8 `MAIN`, 3
`postfix:<!>`, 2 `infix:<op>`. The `Test::Util`-style helper names in the table above
(`is_run`, `group-of`, `tap-ok`, `assert-eq`) do **not** reach this site; they belong to
`exec_call`'s 48.

`compile_and_call_function_def` turned out to be the wrong entry for these callers too,
for a weaker reason than the deferral chain's: they have already resolved their
candidate, so its per-call multi-dispatch frame is pure overhead. It A/B'd measurably
worse than `call_compiled_function_named`.

Two blockers keep C6d-1 open, both narrow:

1. **Multi candidates never get plan-attached bytecode** — `vm_register_sub_ops` has
   `if *multi { continue; }` where it would attach a compiled routine key to a candidate
   `FunctionDef`. That is why `multi_candidate_state_forces_interpreter` has to exist and
   why `call_function_def` survives. Own ticket:
   `todo/tickets/attach-plan-bytecode-to-multi-candidates.md`.
2. **`calls.rs:exec_call`** still carries an inlined copy of `call_function_def`'s whole
   body, `run_block(&def.body)` included.

## Suggested subdivision

Mirroring how C6 was subdivided:

- **C6d-1 — the ordinary-routine tail** (`calls.rs:call_function_def`,
  `calls.rs:exec_call`): 192 hits, ~37 names, dominated by `where`-multi + re-dispatch.
  Pick the block-chunk-vs-routine-convention shape here first, since this is where the
  argument-binding conflict lives.
- **C6d-2 — grammar token/rule bodies** (`dispatch.rs:eval_token_def`,
  `regex_token_resolve.rs`): 83% of the hits, but a regex-execution-model question;
  scope against ADR-0009 rather than the OTF gate.
- **C6d-3 — prove the two dead sites dead** (`call_proto_dispatch`,
  `run_role_submethod`), and move `run_role_submethod` to Phase D where its `MethodDef`
  belongs.
