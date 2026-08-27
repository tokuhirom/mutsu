# A routine-local `:=` bind no longer writes through to a same-named caller lexical

Binding a routine-local declaration to an alias and writing through the alias
used to leak the write into an enclosing scope's *same-named* lexical, which
the routine never touched:

```raku
my $q = "OUT";
sub m { my $q = 5; my $r := $q; $r = 9; $q }
m();
say $q;      # raku: OUT     mutsu (before): 9
```

It reproduced identically with a `method` body instead of a `sub`, and with the
enclosing declaration at mainline or inside a block. The direct forms were fine
— replacing `my $r := $q; $r = 9` with `$q = 9` (or `$q++`, `$q += 4`) left the
outer `$q` alone — so it was specific to the write travelling through the `:=`
alias. It was name-independent, and unrelated to the `$self`/invocant collision
fixed by ADR-0061 (it was found while writing that fix's pinning test,
`t/lexical-self-vs-invocant.t`).

## Root cause

Not the compiler's `Bind`/`AssignOp::Bind` path, as the ticket suspected. It
was the *same* mechanism as
`todo/deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md`,
seen without recursion:
`Interpreter::propagate_bind_to_ancestor_frames` (`src/vm/vm_var_assign_ops.rs`)
splices a `:=` bind's shared `ContainerRef` into every ancestor call frame whose
`saved_env` declares the source **name** in its own tier, so that the binding
survives that frame's env restore on return. A bare name is not an identity:
here the routine's own `my $q` and the caller's `$q` merely share a name, and
the caller's frame got the callee's cell.

## The fix

Fixed together with the recursion case, by gating the splice on the compiler's
own resolution of the bind source rather than on the name — see
`news/2026-08/bind-propagate-ancestor-frames-frame-ownership-gate.md` for the
mechanism. The two shapes are the same bug: one clobbers ancestor invocations
of the same recursive routine, the other clobbers a same-named lexical one
frame up.

Pinned as subtests 9 and 10 of `t/bind-alias-recursive-frame-index.t` (the
alias must still write through *inside* its own routine, and must not reach the
caller's same-named lexical). Both assertions match `raku`.
