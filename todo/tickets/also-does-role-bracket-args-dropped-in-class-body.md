# `also does Role[Args]` inside a class body drops the bracket arguments

`also does RoleName[Args];` inside a class body is supposed to compose a
*parameterized* role the same way a class header `does RoleName[Args]` or a
role-body `does RoleName[Args]` does. In `raku`:

```raku
role R[::T] { method t() { T.^name } }
class Base { }
class Foo is Base { also does R[Int]; }
say Foo.new.t;   # raku: Int
```

mutsu instead prints `Str` — the type parameter silently defaults away
instead of binding to `Int`.

Root cause: `also_trait_stmt`'s `does` arm
(`src/parser/stmt/class/class_decl.rs:598-608`) parses the role name with
`parse_token_like_name`, which does not consume a following `[...]` bracket
suffix at all (unlike every other `is`/`does`/`hides` arm in the same file,
which all call `parse_optional_bracket_suffix` and append the bracket text
onto the parent/role name — see `class_decl.rs:416`, `459`, `474`, `486`).
The bracket text is left as trailing, unconsumed source, and
`Stmt::DoesDecl { name }` carries only the bare role name.

At registration, `class_body_does_decl`
(`src/runtime/registration_class_body_does.rs:49`) looks the role up in
`registry().roles` by that bare name directly — it never calls
`resolve_role_candidate`/`eval_role_arg_values` (`registration_role.rs`) the
way the class-header and role-body `does` forms do, so even if the parser did
capture the bracket text, this call site would still need to route through
role-candidate resolution to pick the right parametric candidate and bind its
type arguments.

Two independent gaps to fix together:
1. Parser: `also_trait_stmt`'s `does` arm needs `parse_optional_bracket_suffix`
   like its siblings.
2. Registration: `class_body_does_decl` needs to resolve a parametric role
   candidate (via `resolve_role_candidate`) instead of a bare
   `registry().roles.get(name)` lookup, and thread the resolved type
   parameters into the composed attributes/methods the way
   `compose_role_into_class` already does for the header form.

Found during ADR-0019 D4 scoping (2026-08-08); unrelated to the ADR's own
declaration-plan migration — this is a plain correctness bug independent of
which registration mechanism (AST walk vs. compiled plan) eventually reads
it.

**The fix is bigger than the two gaps above once traced further.** The
class-header composition path (`compose_role_into_class`,
`registration_class_compose.rs:125-330`) does much more than
`class_body_does_decl` even for a *non*-parametric role: it carries forward
each composed role's class-level attributes (`role_class_level_attrs`),
attribute default expressions (`role_attribute_default_exprs`), `is Type`
container traits (`role_attribute_is_types`), declared attribute type
constraints (`role_attribute_types`), and attribute smileys
(`role_attribute_smileys`) onto the consuming class, substituting any role
type parameters into method signatures/bodies along the way
(`substitute_type_params_in_method`). None of that carryover exists in
`class_body_does_decl` — it only copies `attributes`, `methods`, and
`wildcard_handles`. So `also does SomeRole;` inside a class body is already
missing several role-attribute features today, independent of the bracket-arg
bug above; a correct fix needs to either route `class_body_does_decl` through
`compose_role_into_class` itself (currently private to
`registration_class_compose.rs`, would need `pub(super)`) or reimplement the
same carryover logic. This is a substantially larger change than the parser
one-liner it might look like at first glance — scope it as its own
multi-part investigation, not a quick fix.
