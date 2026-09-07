# `.^mro` no longer lists a class's composed roles

```raku
role R2 { }
class K does R2 { }
say K.^mro.map({ .^name }).join(",");
# raku:  K,Any,Mu
# mutsu: K,R2,Any,Mu
```

Everything else about the composition already answered as raku does — `.^roles`
reported the role, its methods were found on the type object and on an
instance, `K.isa(Any)` and `K.isa(Mu)` were both True, and plain class
inheritance linearized correctly. Only the MRO *listing* disagreed.

## Why the roles are in there, and why the filter is faithful

mutsu keeps composed roles in a class's `parents`, and that list **is** the
method-resolution walk: a role's methods are found by walking it. Rakudo
composes them *into* the class instead, so its MRO does not need them.

The ticket was right to warn that filtering the output while leaving the walk
intact could make introspection and dispatch disagree behind rakudo's back. It
does not, because **rakudo has exactly the same split and exposes it through an
adverb**:

```
K.^mro          # K, Any, Mu
K.^mro(:roles)  # K, R2, Any, Mu
```

The second is precisely what mutsu's plain `.^mro` had been returning. So the
fix is one filter at the plain `.^mro` site, and the `:roles` path is left
alone — mutsu already answered that one correctly.

## The mechanism was already here

`class_does_only_roles` records which of a class's `parents` entries are
purely-`does`-composed roles, and `.^mro_unhidden` has always used it for the
same distinction (which is why `.^mro_unhidden` already answered `K, Any, Mu`).
`.^mro` simply never applied it. The new
`classhow_mro_names_without_does_roles` factors that filter out of the
unhidden path's inline loop.

The distinction matters: an **`is Role` pun stays**. `class C is R { }` is
`C, R, Any, Mu` in rakudo and here, and `class_does_only_roles` does not record
a user-written pun.

It did record rakudo's own one, though. `X::TooLateForREPR` has `X::Comp` as
**both** its declared parent and a composed role — the single documented
exception to "a marker role name never appears in a class's `.^mro`", verified
against real raku — and `register_x` was writing it into
`class_does_only_roles` regardless. `.^mro_unhidden` had been silently dropping
`X::Comp` from that class ever since (a pre-existing divergence this change
surfaced and fixes), and the new `.^mro` filter would have dropped it too.
`register_x` now excludes a role that is also the class's declared parent from
the does-ONLY set, which is the same rule the user-written pun already
satisfies by construction. `t/exception-role-membership.t` caught it.

## Pins

`t/mro-excludes-composed-roles.t` — new, 19 assertions, **each also passing
under rakudo v2026.07**: a single composed role, two of them, one composed by a
parent class, and one composed into another role, all absent from the plain
MRO; the `:roles` adverb still listing them; the `is Role` pun still present;
`X::TooLateForREPR` keeping `X::Comp` in both `.^mro` and `.^mro_unhidden`
while `.^roles` still reports it;
ordinary inheritance unchanged; and the six things the MRO walk still has to
answer (`.^roles`, a role method on the type object and on an instance,
`.isa(Any)`, `.isa(Mu)`, and the `does` smartmatch on both).

The 121 whitelisted `roast/S12-*` and `S14-*` files (2395 assertions) are
green, as is `make test`.

## Residual

`K2.^mro(:roles)` for `class K2 does R2 does R3` lists the roles in declaration
order where raku lists them last-declared first (`K2,R3,R2,...`). One role and a
role-in-role both agree, so it is only two roles composed by the same class, and
it is in the `:roles` variant rather than the plain one. Filed as
`todo/tickets/mro-roles-adverb-lists-roles-in-declaration-order.md`, with the
caution that the same registration order may decide method precedence between
the two.
