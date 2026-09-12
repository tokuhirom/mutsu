# A compound-name role declaration installs into its package, and `Awaitable` is a role again

Two small core-surface gaps, both found by the `invalid-typename` ecosystem cluster
([#7993](https://github.com/tokuhirom/mutsu/issues/7993)) and both blocking `TAP` — and through
it `App::Mi6` and `Mi6::Helper` — at load time.

## A `role` declared with a compound name was not qualified by its package

`exec_register_class_op` has qualified a nested declared name by the enclosing package for a while:
`module M { my class A::B { } }` declares `M::A::B`, which is what rakudo does for `my`, `our` and
a bare declarator alike. The *role* arm of the same op still carried the older rule — any name
containing `::` was taken to be already fully qualified and registered verbatim — so
`module M { my role A::B { } }` registered a global `A::B` instead of `M::A::B`.

The visible symptom was `X::InvalidType` on the module's own code. `TAP.rakumod` is exactly this
shape:

```raku
unit module TAP:ver<0.3.15>;
role Entry { }                                    # TAP::Entry
my role Entry::Handler { ... }                    # installs Handler into TAP::Entry
my class State does TAP::Entry::Handler { ... }   # line 654
```

Line 654 died with `Invalid typename 'TAP::Entry::Handler'`, because nothing was registered under
that name: the role had gone in as a bare `Entry::Handler`. The fix is to give the role arm the
class arm's rule — qualify unless the name is already prefixed with the current package — after
which the qualified spelling, the relative spelling (`Entry::Handler`) and the package walk
(`TT::Entry.WHO<Handler>`) all name one role, and `.^name`, `===` and `.HOW` agree with rakudo for
every scope declarator.

## `Awaitable` was not in the core role list

With the name resolving, `TAP`'s next line was `class Parser does Awaitable`, which failed the same
way. `Awaitable` is a core role — `raku -e 'say Awaitable.^name'` resolves it with no `use`, and
mutsu's own builtin type catalog already lists it as a role of `Promise` and `Channel` — but it was
missing from `BUILTIN_ROLE_NAMES`, the list every `does`-validation and `.HOW` consumer ORs against
the registry. So the name decayed to a bareword `Str` and the composition was rejected.

Adding it makes a user class compose `Awaitable` the way rakudo allows, with `~~`, `.^roles` and
`.HOW` matching. Two related gaps stay open and are filed separately: `Promise ~~ Awaitable` still
answers `False` because the catalog's `roles` column is not yet read by anything
([#8118](https://github.com/tokuhirom/mutsu/issues/8118)), and `await` on a user class does not
consult its `get-await-handle`.

## Effect on the corpus

`TAP` moves off `blocked_load`: `use TAP` now succeeds, and `t/string.rakutest` runs 17 of its 61
assertions instead of dying on the first line of the module. What it stops on next is unrelated to
typenames — a role's `::?CLASS:U:` multi candidate is dropped when the role is composed
([#8119](https://github.com/tokuhirom/mutsu/issues/8119)), and TAP's own subtest entries come back
as `TAP::Unknown`. The cluster's own re-measure ([#7993](https://github.com/tokuhirom/mutsu/issues/7993))
will re-group the rest.
