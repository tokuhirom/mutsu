# `.^mro` lists a class's composed roles; raku's does not

Measured 2026-09-06 against raku v2026.07 and a debug `mutsu` on `main` at
`06d287ac1`, while verifying the `.isa`-on-roles fix in
`news/2026-09/...crane-array-path-semantics` (PR #7387). That fix is correct;
this is a separate divergence found next to it.

## Repro

```raku
role R2 { }
class K does R2 { }
say K.^mro.map(*.^name).join(",");
# raku:  K,Any,Mu
# mutsu: K,R2,Any,Mu
```

Two roles compose the same way — raku `K,Any,Mu`, mutsu `K,R2,R3,Any,Mu`.

## What is already correct, and why that matters here

Everything else about role composition answers as raku does:

| Program | raku | mutsu |
|---|---|---|
| `K.^roles.map(*.^name)` | `R2` | `R2` |
| `K.f` / `K.new.f` for `role R2 { method f {...} }` | `r` | `r` |
| `K.isa(Any)` / `K.isa(Mu)` | `True` `True` | `True` `True` |
| `class D is B { }; D.^mro` | `D,B,Any,Mu` | `D,B,Any,Mu` |

So this is not a composition bug: the role's methods are found, `.^roles`
reports it, plain class inheritance linearizes correctly. Only the *MRO listing*
disagrees.

## Why this is a ticket and not a one-line filter on `.^mro`

**`class_mro` is not an introspection API in mutsu — it is the dispatch order.**
The most likely reason roles are in there at all is that role methods are
resolved by walking it. Filtering roles out of `.^mro`'s *output* while leaving
them in the walk would make the two disagree, which is the kind of split that
later reads as a mystery; removing them from the walk needs the role methods to
be found some other way first (raku composes them *into* the class, which is why
its MRO does not need them).

There is also a specific hazard on record for exactly this kind of change:
`session-todo-tickets-round133` records an MRO widening — done purely so a
smartmatch would answer `True` — that silently rerouted an unrelated
hardcoded-class-list dispatch table through an ancestor's generic handler and
produced a `Promise` that was never kept. MRO edits in this codebase have a
blast radius well beyond the type they name.

## Suggested approach

1. Find every consumer of `class_mro` / `mro_readonly` and classify each as
   "wants the dispatch order" or "wants raku's MRO".
2. Decide whether roles can be composed into the class's own method table at
   registration time (raku's model) rather than being reached through the MRO
   walk. If they can, `.^mro` needs no filter at all and the divergence closes
   by construction.
3. If they cannot, the honest fix may be a separate `mro_for_introspection`,
   and this ticket should say so with the measurements that forced it.

## Acceptance

The repro prints `K,Any,Mu`; `.^roles`, role-method dispatch, `.isa`, plain
inheritance MRO, and `does`-based smartmatch all stay as they are; a `t/` pin
covers a single role, two roles, a role that a parent class composes, and a
role composed into a role.
