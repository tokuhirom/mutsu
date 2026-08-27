# `:=` ancestor-frame propagation is gated on the compiler's own resolution of the bind source, not on the bare name

A `:=` bind whose source is a lexical the *current* invocation declared itself
used to splice its shared `ContainerRef` cell into every ancestor call frame
that happened to own the same name. Under recursion that silently aliased what
Raku scopes as N independent lexicals into a single cell — a Tier S soundness
bug (silent cross-frame data corruption). It also leaked a routine-local bind's
write out to a same-named lexical in the caller, with no recursion involved.
Both are fixed.

## The bug

```raku
my @levels;
sub rec(Int $n) {
    my $v = $n;
    if $n > 0 {
        rec($n - 1);
    } else {
        my $x := $v;
        $x = 999;
    }
    @levels.push($v);
}
rec(3);
say @levels;
```

`raku` answers `[999 1 2 3]` — only the base case's own `$v` is touched;
`rec(1)`, `rec(2)` and `rec(3)` each keep their own `$v`, per ordinary lexical
scoping. mutsu answered `[999 999 999 999]`.

The same mechanism, without recursion (filed separately as
`todo/tickets/routine-local-bind-writes-through-to-same-named-outer-lexical.md`
and resolved by this change):

```raku
my $q = "OUT";
sub m { my $q = 5; my $r := $q; $r = 9; $q }
m();
say $q;      # raku: OUT   mutsu (before): 9
```

## Root cause (confirmed under `rust-gdb`, not assumed)

`Interpreter::propagate_bind_to_ancestor_frames` (`src/vm/vm_var_assign_ops.rs`)
walks `self.call_frames` in reverse and, for every frame whose `saved_env`
declares the bind's source name in its own tier (`Env::contains_key_own_tier`),
overwrites that entry with the bind's shared cell. The mechanism itself is
load-bearing: a bind performed several frames below the declaring scope must
survive the declaring frame's env restore on return, which is what
`t/bind-source-tracks-through-call-chain.t` pins.

The defect was that a bare name is not an identity. Breaking on the splice line
with `rust-gdb -batch` showed the loop firing with `name = "v"` and
`self.call_frames.len() = 4`, inserting into **three** ancestor frames — one per
outer `rec` invocation, each of which had legitimately declared its own `$v`.
For an ordinary closure-capture bind there is exactly one true declaring scope
on the stack, so the name match is harmless; under recursion, or merely with a
same-named caller lexical, it is not.

## The fix — gate the splice on the compiler's own resolution of the source

`Interpreter::bind_source_is_own_frame_lexical` decides whether the bind's
source is a lexical *this* invocation declared; when it is, nothing is spliced
into any ancestor frame.

The primary signal is a genuine identity token rather than a name: the
`WrapVarRef` site records how the **compiler** resolved the source, and that
resolution rides on the `VarRef` value the bind consumes
(`Value::varref_slot`). A real slot index means the source compiled to a
`GetLocal` of this very code unit; the `u32::MAX` sentinel is the compiler's
explicit "known NOT a local of this frame" (the read compiled to `GetGlobal`),
which is exactly the free-variable case the propagation exists for. The
sentinel is already trusted verbatim elsewhere for the same reason (see
`exec_wrap_var_ref_op` and `t/list-alias-shadowed-name.t`). The three bind
handlers (`vm_var_assign_set_local.rs`, two branches; `vm_exec_dispatch.rs`'s
`SetGlobal` bind handler) now capture the slot before the `VarRef` wrapper is
stripped and pass it in.

Two refinements make the gate honest:

* **When the sigilless alias chain redirects the bind to a different resolved
  source**, the recorded slot describes the wrong name, so the gate falls back
  to a name-based conjunction of two signals that each cover the other's
  failure mode: `code.locals` contains the name (that table is function-wide,
  so a `my $v` in a never-executed *sibling* block satisfies it on its own),
  **and** the name is declared in the current env's own overlay tier (the exact
  mirror of the test the ancestor loop applies — but a frame whose env got
  flattened past `MAX_OVERLAY_DEPTH` carries inherited names in its own tier,
  which `code.locals` rules out because a free variable has no slot here).
* **A parameter slot is not a fresh lexical.** A raw (`\p`) or `is rw`
  parameter aliases the caller's container, and mutsu currently carries that
  outward reach through this very splice, so a source living in a
  `code.param_local_slots` slot is deliberately *not* treated as "mine". Only a
  `my`-declared local is unambiguously this invocation's own. Without that
  exception, `roast/S32-list/tail.t` and `skip.t`'s `PredictiveIterator`
  subtests regress — see below.

The flag must be read *before* the handler writes the container into the env
under the source's name, so all three call sites compute it up front.

## What deliberately did not change, and why

The loop still patches **every** matching ancestor frame rather than stopping
at the innermost one. Stopping there is the semantically right rule (that frame
is the tier the env chain's own lookup resolves to, since a callee's env is an
`Env::scoped_child` of its caller's) and was measured to fix one further shape
— a bind performed from a closure nested inside a recursive routine. But it
breaks a raw-parameter alias *chain* (`method new(\p) { self.bless!SET-SELF: p }`
→ `method !SET-SELF(\p) { $!x := p }`), because mutsu does not propagate a bind
transitively through each raw parameter's own aliasing; the blanket by-name
write is what currently carries the cell to the outermost frame. Both that
restriction and the parameter exception above are recorded, with the mechanism
that would lift them, in
`todo/tickets/bind-alias-chain-through-raw-params-blocks-innermost-frame-splice.md`.

## Coverage

`t/bind-alias-recursive-frame-index.t` previously pinned mutsu's *buggy*
`[999 999 999 999]` output on purpose, so this ticket's fix would have a test to
flip green; it now asserts raku's answers and was extended from 2 to 10
subtests: the scalar and whole-container (`@`) recursive binds, mutual
recursion between two subs declaring the same local name, a recursive
**method**, a genuine captured free variable bound from deep inside a recursion
(the case the mechanism exists for — it must still track its source, and must
leave every level's own local untouched), `$x := @a[0]` and `$x := %h<k>`
element binds inside a recursive routine, and the non-recursive
routine-local-versus-caller shape. The whole file passes under `raku` as well
as mutsu.

`t/bind-source-tracks-through-call-chain.t`, `t/bind-alias-reverse-write.t` and
`t/list-alias-shadowed-name.t` stay green, as does the full local `make roast`.
