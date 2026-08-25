# `anon class`/`anon role`/`anon grammar NAME` still installs NAME in the namespace, and re-declaring NAME yields the same type object

Split out of `anon-class-sub-non-ascii-name-and-sub-gist.md` while closing that ticket. The two
bugs that ticket named are fixed; this is the residual it surfaced.

`anon` is supposed to keep the declared name *on the type object* (`.^name`, gist `(Foo)`) while
installing **no symbol** anywhere — that is the whole point of the declarator (see
`raku-doc/doc/Language/variables.rakudoc`, "The `anon` declarator"). `anon sub NAME { ... }` gets
this right in mutsu today: it is marked `__anon_decl` by the parser and
`compile_anon_named_sub_decl` builds the routine value without registering `&NAME`. The *package*
declarators do not have an equivalent — `anon_class_expr` / `anon_role_expr` / `anon_grammar_expr`
emit an ordinary `Stmt::ClassDecl` / `RoleDecl` / `Package`, which registers the name globally.

## Repro 1 — the name leaks into the namespace

```
my $a = anon class Foo {}; say Foo;
```

- raku: `===SORRY!=== Undeclared name: Foo used at line 1`
- mutsu: `(Foo)`

Same for `anon role Foo {}` and `anon grammar Foo { token TOP {.} }`. mutsu has no general
compile-time undeclared-name check, so the *observable* divergence is limited to the name being
resolvable when it should not be — but it is the same underlying leak.

## Repro 2 — two `anon class NAME` declarations share one type object

```
my $a = anon class Foo {}; my $b = anon class Foo {}; say $a === $b;
```

- raku: `False` (each `anon class` is a fresh, distinct type that merely carries the same name)
- mutsu: `True`

Both declarations register into the same name-keyed slot, so the second overwrites the first and
both variables observe one type. A test that declares an `anon class` per loop iteration and
expects distinct types will silently get one.

## Why this is not a one-liner

The registry is name-keyed: `RegisterDecl` installs the class/role/grammar under its (package
qualified) name, and `.new`, method resolution, `::?CLASS`, and type-object identity all find it
by that key. Making an anon package genuinely uninstalled means giving the declaration a
registry-unique internal key while keeping the *display* name (`.^name`) as declared — i.e. the
same split `__ANON_CLASS_{id}__` already uses for the unnamed form, but with a user-visible name
attached. That touches registration, name resolution, and introspection together, so it wants its
own slice rather than being bolted onto a parse fix.

## What is already fixed (do not re-do)

- Non-ASCII names after `anon class` / `anon sub` / `anon grammar` parse (the expression-position
  package parsers use `is_raku_identifier_start`, not an ASCII-only class).
- `anon sub NAME` gists as `&NAME`.
- `anon class NAME { }` twice in one scope no longer raises a false `X::Redeclaration` — the
  parser marks it `__anon_decl` and the compiler's per-scope class check skips it.

Pin for the fixed behavior: `t/anon-declarator-name-and-gist.t`, `t/anon-sub-name-gist.t`.

## Affected files

- `src/parser/primary/misc/anon_decl.rs` — the three expression-position package parsers
- `src/parser/primary/ident/identifier_call.rs` — the `"anon"` arm that marks `__anon_decl`
- `src/compiler/stmt.rs` — `Stmt::ClassDecl` compilation / `RegisterDecl`
- `src/runtime/registration_class*.rs` — name-keyed registration
