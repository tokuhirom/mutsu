# A role header's `is Trait` reaches `trait_mod:<is>`, whatever its case

Raku spells a declarator-level trait `is Foo`, and decides what that means from
whether `Foo` names a known type — never from its capitalisation. If it does not
name a type, `is Foo` desugars to the named argument `trait_mod:<is>($type,
:Foo)`. The class side learnt that when the `Staticish` distribution's
`class Foo is Static { }` was fixed; the role side kept the older, wrong rule:

```raku
multi sub trait_mod:<is>(Mu:U $doee, :$Marked!) { say "trait fired on " ~ $doee.^name }
role Rr is Marked { }
```

| | output |
| --- | --- |
| `raku` | `trait fired on Rr` |
| `mutsu` | `Unknown role: Marked` |

`registration_role_body.rs` deferred an unresolvable parent name to custom trait
dispatch only when the name began with a lowercase letter — so `role Rr is
marked { }` worked and the overwhelmingly common capitalised spelling did not
([#8100](https://github.com/tokuhirom/mutsu/issues/8100)).

## Why the gate could not just be deleted

The class path can widen the rule freely because it knows which parents were
written with `does`: `validate_class_parents` takes `parents` and
`does_parents` as separate lists. The role path knew nothing of the sort. The
parser folds a role header's `does Parent`, `is Parent` and `hides Parent`
clauses into the *same* synthetic `Stmt::DoesDecl` statements prepended to the
body, and `RoleParentOp` — the typed plan op the role-body walk reads them back
through — carried `name`, `hides` and `args` but no declarator.

Dropping the lowercase gate as-is would therefore also have widened `does`:
`role R does NoSuchRole { }` would have stopped being `Unknown role:
NoSuchRole` and become `X::Inheritance::UnknownParent` via the no-candidate
fallback — an inheritance error for a composition typo.

So the declarator is now carried end to end: `Stmt::DoesDecl` gains a
`from_is` flag (set by both role-header parsers, the block form in
`role_decl.rs` and the `unit role` form in `package_decl.rs`), `decl_plan.rs`
copies it onto `RoleParentOp`, and the deferral widens for `is` parents only.
The `does` arm keeps the narrow lowercase rule it had, so its typo diagnostic is
byte-identical to before.

## What now works

`role Rr is Marked { }` fires the user's handler, the role composes into a class
with its methods intact, and the trait name stays out of `.^roles` (it is a
trait, not a parent — rakudo reports zero composed roles there too). A genuine
`is` typo still raises `X::Inheritance::UnknownParent` with the same message
rakudo prints, because the dispatch site in `vm_typedecl_ops.rs` turns a
no-matching-candidate result back into `unknown_parent_error`. The `unit role`
spelling is covered by the same change and was measured against `raku` with the
trait exported from a second module (a `unit role` cannot define its own
`trait_mod` before its header).

Pinned by `t/oo/role/uppercase-is-trait-reaches-trait-mod-on-a-role.t` (10
assertions, green under `raku` as written) — the role-side mirror of the
class-side `t/oo/trait/uppercase-is-trait-reaches-trait-mod.t` — and by a parser
unit test asserting the `from_is` flag itself for both spellings.
