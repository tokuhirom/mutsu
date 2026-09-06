# A trailing comma in an attribute default silently drops the initializer (and sometimes the accessor)

Found 2026-09-06 while writing a regression pin for the attribute-default
closure-package fix (`news/2026-09/attr-default-closure-declaring-package.md`).

A trailing comma is legal in every Raku list literal, including the one that
initializes an attribute. mutsu parses the declaration but throws the value
away, and for a `%`-sigil attribute it loses the generated accessor as well:

```raku
class C { has @.a = 1, 2,; has $.b = 3; method m { @!a } }
say C.new.m.raku;   # raku: [1, 2]     mutsu: []
say C.new.b;        # 3 in both — an adjacent attribute is unaffected
```

```raku
class C { has %.t = a => 1, b => 2,; }
say C.new.t.raku;   # raku: {:a(1), :b(2)}
                    # mutsu: No such method 't' for invocant of type 'C'
```

Removing the trailing comma makes both correct, so the value and the accessor
are both a function of how the initializer expression is scanned:

```raku
class C { has %.t = a => 1, b => 2; }
say C.new.t.raku;   # {:a(1), :b(2)} in both
```

The same trailing comma is fine outside a class body (`my @x = 1, 2,;` gives
`[1, 2]` in mutsu), so this is specific to the attribute-declaration parse, not
to list literals in general. The `%`-sigil case losing the accessor suggests the
attribute declaration itself fails to register (the `has` statement is being
mis-terminated at the comma), rather than only its default expression being
mis-evaluated.

Affected area: the `has` declaration parser in `src/parser/` and the
`CompiledAttrDecl` default lowering it feeds (`src/opcode.rs`
`CompiledAttrDecl { default: Option<DeclTraitArg>, … }`, evaluated by
`Interpreter::eval_attr_default_expr` in `src/runtime/attr_build_defaults.rs`).

Minimal repro: the two snippets above. A pin belongs next to
`t/attr-default-closure-package.t`, whose fixture module had to drop a trailing
comma to work around this.
