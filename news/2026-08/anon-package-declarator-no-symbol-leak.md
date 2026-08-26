# `anon class`/`anon role`/`anon grammar NAME` no longer install a symbol anywhere

`anon` is supposed to keep a declared name on the type object (`.^name`, gist `(Foo)`) while
installing **no symbol** anywhere — that is the whole point of the declarator (see
`raku-doc/doc/Language/variables.rakudoc`, "The `anon` declarator"). `anon sub NAME { ... }`
already got this right in mutsu; the *package* declarators (`anon class`/`anon role`/
`anon grammar`) did not:

- **Repro 1**: `my $a = anon class Foo {}; say Foo;` printed `(Foo)` in mutsu (raku: compile-time
  "Undeclared name" error).
- **Repro 2**: `my $a = anon class Foo {}; my $b = anon class Foo {}; say $a === $b;` printed
  `True` in mutsu (raku: `False` — each `anon class Foo {}` is a fresh, distinct type that merely
  shares a display name).

## Root cause, and where the original ticket's analysis was wrong

The ticket (`todo/tickets/anon-package-declarator-still-installs-its-name.md`) predicted the fix
would need changes to `src/compiler/stmt.rs`'s `RegisterDecl` compilation and to
`src/runtime/registration_class*.rs`'s name-keyed registration, reasoning that "the registry is
name-keyed... making an anon package genuinely uninstalled means giving the declaration a
registry-unique internal key... that touches registration, name resolution, and introspection
together."

That turned out to be unnecessary. Every place that installs a bareword/alias/stash entry for a
class/role/package declaration (`exec_register_class_op` in `vm/vm_typedecl_ops.rs`,
`exec_register_role_op`, the `RegisterPackage`/`RegisterPackageMy` VM ops, `qualify_decl_name`,
`qualify_package_name`, `package_type_aliases`, `resolve_lexical_type_key`'s `\u{0}`-prefix probing,
...) keys **entirely off the declaration's `name: Symbol`, treated as an opaque string** — none of
it special-cases "is this anon" or "was this already installed." That means mangling the `name`
Symbol itself, once, at **parse time**, makes every one of those call sites automatically install
a key nobody can type as a bareword (`\u{0}` cannot appear in Raku source text) — with **zero
changes** to the compiler's `RegisterDecl` emission or to any runtime registration function.

This is exactly the scheme ADR-0047 already uses for `my class Foo {}`'s *storage name*
(`Foo\u{0}<decl-id>`, stripped back to `Foo` for display by
`value::display::user_facing_type_name`), reused here for the *whole* declaration rather than a
separate storage indirection. The fresh id comes from the same global `next_class_decl_id()`
counter already used for `decl_id`, drawn once per **parse** of the `anon class`/`anon role`/
`anon grammar` expression — so two textually distinct declaration sites always mangle to two
distinct keys (repro 2's `False`), while the same site re-executed (a loop body, a sub called
twice) reuses its one fixed mangled name and therefore its one type identity (matching Rakudo,
verified against `raku`: `for 1..3 { @a.push(anon class Foo {}) }; say @a[0] === @a[1]` is `True`
in both).

The one thing left alone on purpose: a genuinely **unnamed** `anon class { }` (whose own parser
already mints a globally-unique `__ANON_CLASS_<n>__`/`__ANON_ROLE_<n>__`/`__ANON_GRAMMAR_<n>__`
marker, displayed as `<anon|N>`) is never mangled further — doing so would break
`value::display::anon_type_display_name`'s recognition of that exact marker shape.

## The fix

- `src/parser/primary/misc/anon_decl.rs`: new `mark_anon_package_decl()`, called from the `"anon"`
  arm of `src/parser/primary/ident/identifier_call.rs` right after
  `anon_class_expr`/`anon_grammar_expr`/`anon_role_expr` parse successfully. It pushes the
  `__anon_decl` custom trait (already used to skip the same-scope class-redeclaration check) onto
  `Stmt::ClassDecl`/`Stmt::RoleDecl`, and mangles a genuinely-NAMED declaration's `name` Symbol with
  a fresh `\u{0}<site-id>` suffix (a no-op on the already-unique internal `__ANON_*__` marker of an
  unnamed declaration). `Stmt::Package` (the route `anon grammar` takes — it declares a `ClassDecl`
  with `Grammar` default parent for a *statement*-position `grammar Foo {}`, but a `Package(kind:
  Grammar)` node for the *expression*-position form) has no `custom_traits` field and needs only
  the name mangling.
- No compiler or VM/runtime changes at all — the existing registration machinery, applied
  uniformly to every class/role/package name as an opaque string, does the rest.

## What was measured against `raku` before and after

`.^name`/`.gist` of a named `anon class`; `.new` and method dispatch on the constructed instance;
`self.^name`/`::?CLASS` inside a method body; a bareword self-reference from inside the anon
class's own body (raku: "Undeclared name", even self-reference); `anon class Foo` declared inside
`package Pkg { ... }` (must not leak `Pkg::Foo` either); `is NamedParent`/`does NamedRole` composing
correctly; two `anon role Foo {}` (already correctly distinct beforehand, via the pre-existing
per-declaration `role_id` punning mechanism — untouched by this fix); an `anon grammar` with a real
`token TOP` still parsing; and an `anon class Foo` declared alongside an unrelated *non-anon*
`class Foo` in the same scope (the two must stay completely independent: the anon declaration
neither shadows nor collides with the real one). All of these matched `raku` exactly, both before
and after the fix (the two repros were the only divergence).

## Tests

New regression test: `t/anon-package-decl-no-symbol.t` (21 assertions), covering both repros,
package-nesting, the loop-reuse identity case, and the full semantics list above. It runs unchanged
under both `raku` and mutsu — repro 1's mutsu-observable half uses `::('Name')` (a runtime indirect
lookup that both implementations agree fails with a `Failure`) since raku's actual behavior for a
bare reference is a compile-time error that cannot be captured in a TAP assertion.

Existing pins `t/anon-declarator-name-and-gist.t` and `t/anon-sub-name-gist.t` (including the
*non*-`anon`, bare `(grammar Baz { ... }).^name` expression-position form, which must still install
`Baz` globally — only the `anon`-prefixed form gets mangled) stay green.

Targeted roast sweep on the debug binary (consumers of type registration/name resolution):
`roast/S12-*` (class/method/introspection), `roast/S14-roles/`, `roast/S05-grammar/`,
`roast/S02-names-vars/` — 135 files, all passing.
