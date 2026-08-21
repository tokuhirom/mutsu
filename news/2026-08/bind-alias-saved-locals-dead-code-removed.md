# `:=`-bind ancestor-frame propagation's `saved_locals` slot patch confirmed dead and removed

Resolves `todo/tickets/bind-alias-saved-locals-wrong-frame-index.md`, the
follow-up left by `news/2026-08/bind-alias-reverse-write-through-nested-cell.md`.

`Interpreter::propagate_bind_to_ancestor_frames` (`src/vm/vm_var_assign_ops.rs`)
walks ancestor call frames on a `:=` bind and splices the freshly-minted
container into every frame that already owns the bound name. It used to patch
**two** things per matching frame: `frame.saved_env` (by name — correct) and
`frame.saved_locals[i]` (by an index `i` found by searching the CURRENTLY
EXECUTING frame's own `code.locals` — wrong for the general case, since an
ancestor frame's own locals layout has nothing to do with the executing
frame's layout unless the two frames happen to be recursive invocations of
the exact same compiled function).

The ticket left two options: make the index correct (add a per-frame
locals-name table to `VmCallFrame`) or prove the `saved_locals[i]` patch is
unconditionally dead and delete it. This PR took the second, lower-risk path
and confirmed it empirically rather than by static analysis alone:

- Built two variants of the interpreter — one with the `saved_locals[i]`
  patch intact, one with its body deleted — and ran both against a repro
  that specifically targets the one case the ticket flagged as possibly
  live (a `:=` bind performed at the base case of a *recursive* sub, naming
  that same sub's own local, so the propagate loop's by-name match hits
  every ancestor invocation of the same function and the two locals layouts
  genuinely coincide). Both variants produced byte-identical output for both
  the scalar-rebind bind path and the whole-container (`@`) bind path.
- Also re-ran the two tests the ticket named as already masking this bug
  (`t/bind-source-tracks-through-call-chain.t`,
  `t/bind-alias-reverse-write.t`) and the full `make test` local suite —
  unaffected either way.

This confirms the `saved_env` splice (kept, unchanged) plus the "lazy sync"
blocks in both `exec_get_local_op` (`vm_var_assign_local_get.rs`) and
`exec_set_local_op_inner` (`vm_var_assign_set_local.rs`) — which adopt a
`ContainerRef` from `env` back into `locals` on the next read/write of the
name in a frame — are what actually carry a `:=` bind across the call chain
in every case, including the recursive one. The `saved_locals[i]` patch,
and the now-unused `code: &CompiledCode` parameter it needed, were removed
from `propagate_bind_to_ancestor_frames` and its three call sites
(`vm_var_assign_set_local.rs`, two branches; `vm_exec_dispatch.rs`'s
`SetGlobal` bind handler). Pinned by the new
`t/bind-alias-recursive-frame-index.t`, which exercises exactly the
recursive-bind scenario the ticket was unsure about.

## A separate bug discovered along the way

Building the recursive-bind repro to prove the above surfaced a **different,
pre-existing** bug in the `saved_env` splice itself (unaffected by this PR,
reproduces identically before and after): for a recursive function that
re-declares a same-named local at every call depth, the splice's
`frame.saved_env.contains_key_own_tier(name)` check matches by name only, so
it also splices the newly-bound container into every unrelated ancestor
invocation's own separate same-named local — not just the one true
declaring frame of a captured free variable. `raku` gives `[999 1 2 3]` for
the ticket's repro; mutsu gives `[999 999 999 999]` both with and without
the `saved_locals[i]` patch. This is out of scope here (the sibling ticket
explicitly scoped itself to the `saved_locals` half only) and is filed
separately as
`todo/deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md`.

A second, unrelated, more severe bug was also found and left unfixed
(reproduces on `main`, independent of `:=` bind or block scoping — both were
red herrings from the initial repro shape): a recursive sub that builds a
trailing-comma list literal of its own parameter into a `my @` local
(`my @v = ($n,);`) and never reads that local again before the recursive
call returns crashes with a native Rust stack overflow, even at recursion
depth 1. `t/bind-alias-recursive-frame-index.t`'s whole-container subtest
avoids the crash because it reads `@v[0]` on every call (via
`.push(@v[0])`), which turned out to be load-bearing. Filed separately as
`todo/deep/recursive-sub-trailing-comma-array-literal-of-own-param-stack-overflow.md`,
with a suspected root cause pointing at the array whole-container-identity /
circular-reference-fixup helpers in `src/vm/vm_var_assign_ops.rs`.
