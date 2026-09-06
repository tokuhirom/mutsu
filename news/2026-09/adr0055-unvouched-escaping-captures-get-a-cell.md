# An escaping capture the creating frame cannot vouch for now gets a shared cell

ADR-0055's headline defect — *a closure's free variable resolves through the
dynamic caller chain instead of its own binding* — is closed for plain scalars,
on every invocation path, without touching a single merge policy's default.

## The defect

Six lines, `raku` says `OUTER`, mutsu said `CALLER`:

```raku
sub noop($v) { 1 }
my $b = "OUTER";
noop($b);                      # load-bearing: the vouch refusal
my $f = { $b };
sub collide() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
say collide();
```

Both added lines earn their place. `noop($b)` puts `$b` in
`own_call_arg_sources`, so `CompiledCode::compute_free_vars` refuses to vouch
for it (an `is rw` parameter could write it back, which would make a by-value
overwrite-install go stale). `my $g = { $b }` forces the *caller's* `$b` out of
its local slot and into `env`, where the merge's chain probe can see it. `$b` was
therefore simultaneously not authoritative and not boxed — both defences off —
and a same-named lexical in whatever frame happened to be calling won.

The slot-resident variant (drop `my $g = { $b }`) answered `OUTER` on `main`, but
only by accident: a compiled caller usually keeps its lexicals in slots, so the
probe found nothing to collide with. That is the `env_dirty` dual store leaking
into a *scoping* decision, not a policy that was right.

## The fix

ADR-0025 slice 2 states the invariant every closure-call merge needs: *an
escaping-captured plain scalar is either **authoritative** (a by-value snapshot
is provably exact) or a shared `ContainerRef` **cell***. It was not exhaustive:
the vouch refuses two shapes (`own_container_writes`, `own_call_arg_sources`)
that `captured_mutated_locals` never sees either.

`CompiledCode::needs_cell_unvouched_locals` is the exact complement of the vouch
within the escaping-captured own set, wired into `box_captured_lexicals` as an
independent trigger (it deliberately does NOT require `captured_mutated_locals`
membership — the whole point is that the mutation analysis never saw the write).
The dichotomy is now exhaustive by construction, and because a `ContainerRef`
capture already takes every merge's force-overwrite branch, the hijack is fixed
with `entry_or_insert_sym` still the merge default. No `merge_all` knob was
added, and no merge default was flipped.

## The third merge nobody had counted

Fixing the compiled path exposed that `.map($f)` still answered `CALLER`.
`eval_map_over_items`' inline fast path (`resolution_map_grep.rs`) keeps its own
closure-env pre-insert, and it was the one merge in the codebase with no
`ContainerRef` exception — so it hid the closure's own cell behind the calling
frame's same-named value. ADR-0055 §1.1 counted two merge policies; there are
three. The map site now makes the same two exceptions as the other two (`self`
is lexical; a cell wins unless the name is a dynamic).

## Why the Cro regression did not come back

`todo/deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md`
recorded that this exact mechanism, prototyped in August, dropped six
Cro::HTTP suites: state leaked between sequential requests on one client, visible
as an accumulating request path. Its own leading hypothesis is the fix that
shipped — `needs_cell_unvouched_locals` excludes the frame's own **parameters**.

A parameter is a fresh binding the *caller* creates on every invocation, so the
`is rw`-writeback hazard the `own_call_arg_sources` refusal guards against does
not apply to it: that refusal is about a local the frame declares and then hands
onward. Giving a parameter a cell instead let two invocations of one routine
share a binding, so every stored closure read the last call's argument. A `my`
declaration cannot do this — its vardecl path clears a stale cell and gives the
redeclaration a fresh binding — but a parameter binding is not a vardecl.

`CompiledCode` did not know its own parameter names (`param_name_syms` lives on
`CompiledFunction`, and covers positionals only), so `param_locals` was added,
populated by `Compiler::declare_param` — the single entry point for parameter
declaration, so named and destructured sub-signature parameters are covered too.

## Verification

`t/closure-capture-cell-dichotomy.t` grew from 11 to 17 assertions and is
byte-identical under `mutsu` and `raku`: the env-resident §1.2(b) repro across
four invocation paths (`.()`, `.map($f)`, a native `sort` comparator, and
invocation inside a callee), the parameter-freshness guard that pins the Cro
regression, and two captures of one name at different depths.

## What still diverges

- A capture of the frame's own **parameter** that was itself handed to a call
  (`sub outer($p) { noop($p); { $p } }`) is still hijackable — it is excluded
  from the cell trigger and the vouch still refuses it. Recorded with its
  measured root cause in
  `todo/tickets/parameter-capture-handed-to-a-call-has-neither-defence.md`.
- The `@`/`%` half of the same family (`my @a; @a.push(3); my $f = -> { @a.elems }`
  read from a frame with its own `@a`) is unrelated machinery — container
  lexicals, ADR-0039 — and diverges independently of any scalar capture. Recorded
  in `todo/tickets/container-lexical-capture-loses-to-same-named-caller-array.md`.
