# A punned role now runs its BUILD and TWEAK submethods

`R.new` on a role puns it into a class and constructs. mutsu's pun path skipped
the role's own `submethod BUILD` and `submethod TWEAK` entirely, so every
attribute they would have initialised stayed unset:

```raku
role R { has %.h; submethod BUILD(--> Nil) { %!h{"a"} = 1 } }
say R.new.h.raku;          # was: {}   raku: {:a(1)}
```

Composing the same role into a class (`class C does R { }`) ran them correctly,
so this was specific to punning. It was the top blocker of the **Cro::Core**
suite: `Cro::Policy::Timeout` is a parameterized role whose `BUILD` fills a
`Real() %.phases` hash from the role's `%phase-defaults` parameter, and Cro
instantiates it as a pun — with `BUILD` skipped, every phase lookup answered
`Any`.

## The pun stopped reproducing construction

The cause was a second implementation. `dispatch_new` recognised a role type
object and built the instance by hand — collecting attributes from the role and
its parents, evaluating defaults, type-checking supplied values, tagging typed
containers — as a parallel copy of what the class construction path does. The
copy had drifted: it never grew the BUILD/TWEAK phases, and it was behind on
`is required` and coercion-typed attributes too.

It is gone. The pun now composes the role into a class of the same name and
re-enters `.new` on *that*, so construction is the ordinary class construction:
attribute seeding, `is required`, coercion types, the BUILD phase, the
initializers BUILD left alone, and the TWEAK phase, in that order. A
re-entrancy guard (`role_pun_construction`) makes the second entry take the
class path instead of recognising the name as a role again, and the pun is
withdrawn afterwards so the name stays a role. Three behaviours came back for
free:

```raku
role T { has Int $.i is required }
try { T.new };            # was: an instance   now: X::Attribute::Required
role U { has Int() $.i }
say U.new(i => "42").i;   # was: "42"          now: 42
role V { has $.x = 5; submethod TWEAK(--> Nil) { $!x = $!x + 1 } }
say V.new.x;              # was: 5             now: 6
```

## Three bugs the delegation exposed

**The pun's methods did not say they came from the role.** The pun class is
built by copying the role's methods into a class shell, and the copies kept
`role_origin: None` — the marker that distinguishes a composed method from one
the class declares itself. The construction phases then found the role's `BUILD`
twice, once as the composed-role submethod and once as an apparently
class-declared candidate, and ran it twice (`submethod BUILD { @!a.push(1) }`
punned to `[1, 1]`). Real `does` composition tags them, which is why it never
doubled; the copying pun now tags them the same way.

**An itemized array type argument was spread.** `R[[1, 2]]` parameterises
`role R[@l]` with one argument, but the subscript spread any array index into
one type argument per element, so it passed two — matching no candidate's arity
at all, which left `@l` bound to the bare `1`. Only a comma list (`R[Int, Str]`)
spreads now; `ArrayKind` already carried the distinction between `(1, 2)` and
`[1, 2]`.

**Withdrawing a pun left its construction plan behind.** Construction puns the
role only for the duration and then drops the `ClassDef` again, but the plan
cached under that name — the one that decides a class is simple enough for the
native fast path — survived. The next `R.new` matched that stale plan before
reaching the role branch at all and built a plain instance with no role mixin
markers, so every method call on it died with "No such method". Only the *first*
construction in a program produced a working object. The withdrawal is now one
helper (`withdraw_role_pun`) that drops the plan with the class, shared by all
three sites that had been spelling it out.

Parameterized puns reach the same construction path as bare ones. The
name-driven route (`ensure_parametric_role_pun_class`) still handles a
parameterisation whose arguments have a faithful spelling in a type name; the
fallback for those that do not — a Hash or Array argument, which is exactly the
Cro case — now binds the matched candidate's type parameters through the real
signature binder and constructs through the pun class, instead of building an
attribute-less instance whose state lived only in mixin markers.

## Pin

`t/role-pun-build-tweak.t` (23 tests), every assertion checked against Rakudo
v2026.06: BUILD and TWEAK on scalar/array/hash attributes, each running exactly
once, their ordering against attribute initializers, named-argument binding,
composition still behaving, the four parameterisation shapes (type, value, hash
and array arguments), and a repeated construction keeping both its role methods
and its BUILD.

The sibling finding — that a pun should not construct *at all* for a plain
method call — is unaffected and still tracked in
`todo/tickets/role-pun-should-not-construct.md`.
