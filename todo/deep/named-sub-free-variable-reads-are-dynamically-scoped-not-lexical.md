# A named `sub`'s free-variable reads are dynamically scoped, not lexical: any later same-named inner-block `my` shadows it for every call made from inside that block

## TL;DR

This supersedes and broadens `todo/tickets/named-sub-reads-enclosing-for-loop-param-dynamically-not-lexically.md`
(deleted by this commit — the root cause is NOT for-loop- or thread-specific).

A `sub` declared at mainline/file-scope level, whose body reads a free
variable (a name it neither declares itself nor takes as a parameter),
resolves that name **dynamically** against whatever the *calling* frame's
env currently holds under that bare name — not **lexically** against the
binding that was visible at the sub's own declaration site. Any later block
(a bare `{ }`, an `if`, a `for` loop, a `start` thread body, ...) that
declares its own `my` of the same name shadows the sub's true lexical
binding for the duration of that block, for every call the sub receives
from code running inside it.

## Repro (no for-loop, no threads — a plain bare block is enough)

```raku
my $client = "outer";
sub helper($u) { $client }
{
    my $client = "inner";
    say helper(0);
}
say helper(0);
```

- `raku`: `outer` / `outer` — `helper`'s `$client` is lexically the
  top-level `my $client = "outer"`; the inner block's `my $client` is a
  completely separate binding in a scope `helper` was declared before and
  cannot see.
- `mutsu` (branch `fix/nested-on-demand-supply-quit-propagation`,
  `811460f4b` + this file): `inner` / `outer` — `helper`'s body reads
  whatever value is currently bound to the bare name `client` in the
  calling frame's env.

The original for-loop/`start{}` repro (kept for reference, exercises the
same bug via the `for -> $client { start { helper($client) } }` shape used
in Cro) still reproduces identically and is a special case of this one — see
`tmp/named-sub-lexical-repro.raku`.

## Root cause (confirmed via `rust-gdb -batch` breakpoints, no code changes made)

1. A free-variable read inside a sub body compiles to `OpCode::GetGlobal`
   (`compile_expr_var`, `src/compiler/expr_helpers.rs:688-695`) when the name
   is not the sub's own local — the SAME opcode used for genuine
   package globals/dynamic vars.
2. Named subs never receive closure treatment. `exec_register_sub_op`
   (`src/vm/vm_register_sub_ops.rs:205-515`) registers the sub into the
   global function registry via `register_compiled_sub_decl` with **no**
   call to `capture_closure_env`/`box_captured_lexicals` (`src/vm/vm_register_ops.rs:500`,
   `:759`) — that machinery only runs for anonymous/pointy-block closure
   *values* (`MakeLambda`/`MakeBlockClosure`). A mainline-declared sub
   therefore carries no record of what its free variables meant at its
   declaration point.
   - There IS a partial exception: a sub declared **inside a block**
     (`block_scope_depth() > 0`, line 431) gets a whole-env snapshot stashed
     under a reserved `BLOCK_LEXICAL_SUB_PREFIX` key — but only so the sub
     stays *callable* after the block exits (registry entry dropped on
     block-scope restore). This snapshot is never consulted for shadow
     protection, and mainline-level subs (`block_scope_depth() == 0`, our
     repro's `helper`) skip this path entirely.
3. At call time, `GetGlobal("client")` falls through
   `get_env_with_main_alias_inner` → `self.env().get(name)`
   (`src/vm/vm_env_helpers.rs:620-685`). `env` is one flat, name-keyed store
   shared by every scope currently on the call stack. The inner block's own
   `my $client = "inner"` writes into the SAME env key `"client"` that the
   outer `my $client = "outer"` occupies (this is expected/correct
   dynamic-scope-like behavior for the block's OWN code — the bug is that a
   *called sub* has no way to see past it back to its own declaration-time
   binding).
4. Confirmed live with `rust-gdb -batch` on `Interpreter::set_env_with_main_alias`
   (`vm_env_helpers.rs:731`, breakpoint `-ex 'break src/vm/vm_env_helpers.rs:731'
   -ex 'run' -ex 'print name' -ex continue`, `--args target/debug/mutsu
   tmp/named-sub-block-scope-repro.raku`): it fires once for `name=="client"`
   right before the inner block's `say helper(0)`, and `helper`'s subsequent
   `GetGlobal("client")` (traced the same way at
   `vm_exec_dispatch.rs`'s `GetGlobal` arm) reads that just-clobbered value.

## Why this is NOT the for-loop/thread-specific bug it first looked like

The original ticket found this via a `start {}` thread-spawn repro inside a
`for -> $client { }` loop and hypothesized a thread-clone-env or
shared-store bug specific to that machinery. **That hypothesis is false**:
the bug reproduces with a fully synchronous call, no `for` loop, no thread,
just a bare block — see the repro above. The `for`/`start` shape is simply
the most common way this collision surfaces in the Cro test suite (per-
iteration loop params and thread-local shadow vars both use the same
bare-name env-mirroring mechanism that any ordinary block `my` also uses).

## Why this is a deep ticket, not a quick fix

This is squarely the Slice F / dual-store class of problem (PLAN.md §6):
`env` is a single flat, name-keyed structure that emulates lexical scoping
through disciplined save/restore around block entry/exit, which works for
code running *directly* inside the shadowing block but breaks for any
*called routine* that needs to see past the shadow back to its own
declaration-time binding.

mutsu already has TWO precedents for exactly this shape of fix, both
"authoritative store consulted BEFORE the ambient env" patterns:

- `package_scope_lexical` (`src/vm/vm_env_helpers.rs:304-353`): protects a
  `package Foo { my $x; sub f {$x} }` block's own `my` from being shadowed
  by anything with the same bare name outside the package block. Populated
  by `exec_package_scope_op`. Explicitly **excludes** `current_package() ==
  "GLOBAL"` (i.e. mainline) — see the `cur.is_empty() || cur == "GLOBAL"`
  gate at both `vm_env_helpers.rs:254` and `:306`.
- `unit_scope_lexical` / `unit_lexicals` (`src/vm/vm_env_helpers.rs:356-420`,
  `src/runtime/run_modules.rs:759-830`): protects a `unit module X`'s own
  file-scope `my` from the *loading scope's* same-named `my` (which
  occupies the identical env key while the module body runs in the
  caller's env — see `run_modules.rs:759-764`'s own doc comment, describing
  precisely our bug's mechanism, just between a module and its loader
  instead of between a block and its enclosing scope). Populated once, after
  the module's mainline finishes, by walking `collect_unit_lexical_names`
  and boxing each into a `ContainerRef` cell (so later mutation through the
  module's own routines stays visible — `run_modules.rs:815-830`). Resolved
  via the calling routine's `frame.lexical_package` (`unit_lexical_slot`,
  `vm_env_helpers.rs:386-420`), which is set at sub-registration time for
  module-declared subs. Also excludes `cur == "GLOBAL"` at both its
  qualified- and bare-name branches (`:396`, `:411`).

Both existing mechanisms deliberately stop at the mainline/GLOBAL boundary
— there is currently **no third mechanism protecting mainline's own named
subs' free variables from mainline's own later block shadowing**. This
ticket is that missing third mechanism.

### Sketch of a fix (not implemented — needs design + a full-suite blast-radius check before landing)

The natural extension is a `mainline_lexicals`-shaped store, but mainline
differs from a `unit module` in one important way: a module's subs are only
ever called *after* the whole module has finished loading (so the "run the
whole body once, then snapshot" pattern from `run_modules.rs` is safe), but
a mainline sub can be called *while mainline is still executing*, so its
free variables must stay LIVE (reflecting mainline's own subsequent
mutations of the same lexical), not a one-time post-hoc snapshot.

Concretely:

1. The compiler already computes `free_var_syms` for every `CompiledCode`,
   including named-sub bodies (`compute_needs_env_sync` →
   `compute_free_vars`, `src/opcode.rs:4244`, unconditional — this is not
   closure-specific plumbing, so no new compiler analysis is needed).
2. At `exec_register_sub_op`, for a sub registered at
   `block_scope_depth() == 0` (mainline, not inside any block) and not in
   `EVAL`: for each of the sub's `free_var_syms` currently resolvable in
   `env`, ensure it is a `ContainerRef` cell (installing one in place if
   not — same box-on-first-need approach as `run_modules.rs:821-825`, and
   the existing `box_captured_lexicals` machinery for closures) and record
   the (name → cell) pair in a new `mainline_lexicals: HashMap<String,
   Value>` map on `Interpreter`. Because mainline can declare at most one
   `my $client` per bare name at its own top level (a second one in the
   same scope is a redeclaration error), this map needs no per-unit/per-
   package keying — unlike `package_lexicals`/`unit_lexicals`, one flat map
   suffices.
3. Give each mainline-registered sub's call frame a marker (a routine-def
   flag, or reuse `frame.lexical_package` with a reserved sentinel such as
   `"MAIN"`) so free-variable resolution knows to consult
   `mainline_lexicals` for THIS call. Consult it in the same "authoritative
   store, checked before `env`" position as `package_scope_lexical`/
   `unit_scope_lexical` (their shared call site — trace both call sites in
   `vm_exec_dispatch.rs`/`vm_var_ops.rs` forward from the two functions
   above) — but gated on that per-frame marker instead of `current_package`,
   so ordinary mainline code (not inside a called sub) keeps reading `env`
   directly and is untouched.
4. Because step 2 installs a shared cell into BOTH `env` and
   `mainline_lexicals`, a later plain mainline `$client = "changed"` (at
   `block_scope_depth() == 0`, not inside a shadowing block) still mutates
   the SAME cell through the normal `env` write path — no special write-side
   handling needed, unlike `unit_scope_lexical_write`'s dedicated write
   companion (that one exists because a module's own routine writing its
   file-scope lexical must NOT touch the loading scope's `env` entry; here
   there is only one scope, so ordinary env writes already land on the
   shared cell once boxed).
5. **Blast radius**: this changes free-variable resolution for potentially
   every mainline-declared named sub in the entire test suite (not just the
   shadowed-name case) if the frame-marking or cell-boxing logic has bugs,
   so this needs the full `make test` + CI `make roast` safety net treated
   as load-bearing, not optional — no cherry-picked local subset. Consider
   whether boxing should be lazy (only when a NAME COLLISION is actually
   detected — e.g. only box/record a free var the first time a same-named
   `my` is about to shadow it in a nested block) rather than eager (box
   every mainline sub's every free var at declaration time, which is
   simpler but touches many more programs and has a real perf cost on the
   declaration hot path for programs with many top-level subs).

## Verification (once fixed)

- Both repros above should print `outer,outer` under mutsu, matching `raku`:
  `tmp/named-sub-block-scope-repro.raku` (new, minimal, no Cro/threads) and
  `tmp/named-sub-lexical-repro.raku` (original `for`/`start{}` shape).
- `make test` full local TAP suite, `make roast` via CI (do not hand-pick a
  subset — see blast-radius note above).
