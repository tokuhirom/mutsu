# A class-body `my class` clobbers a file-scope class of the same name

A lexical class declared inside a `class` body registers its short name into the
*current* env unconditionally, so a same-named class at file scope stops being
reachable after the declaration.

```raku
class Foo { method who() { "outer-Foo" } }
class InClass {
    my class Foo { method who() { "inner-class-Foo" } }
    method make() { Foo.new }
}
say InClass.make.who;   # inner-class-Foo   (both)
say Foo.new.who;        # raku: outer-Foo   mutsu: X::Undeclared::Symbols: Foo
say Foo.^name;          # raku: Foo         mutsu: (never reached)
```

## Root cause

`exec_register_class_op` (`src/vm/vm_typedecl_ops.rs`, the `parent_is_class`
branch) does three things for a class-nested type:

```rust
self.suppress_name(&resolved_name);
self.register_class_scoped_short_name(&resolved_name);
env.insert(resolved_name.clone(), Value::package(Symbol::intern(&storage_name)));
```

Both the suppression and the env insert are global and permanent:

- `suppress_name("Foo")` makes the *file-scope* `Foo` raise
  `X::Undeclared::Symbols` (`exec_get_bare_word_op`'s suppressed-name branch
  does not exempt a `Package`, on purpose — see the `t/class-is-export-tag.t`
  comment there).
- the `env.insert` overwrites the outer binding, so even without the
  suppression `Foo` would resolve to `InClass::Foo`.

A class body is not an env scope in mutsu, so there is nothing that restores the
outer binding when the body ends. The role path does not have this problem
because it uses `entry_or_insert_with` (non-clobbering) — but that is why the
role path had the *opposite* bug, fixed separately by giving role-lexical types
their own `::?ROLE`-anchored probe in `resolve_suppressed_type`.

## Why it is not a one-liner

Simply dropping the env insert is not safe: the entry is also what
`lexical_env_remap_name` reads to bind `is Foo` inside the same body to the
mangled storage name, and what `::("Foo")` indirect lookups find. The real fix
is to make a class body push/pop a lexical scope for its nested type names (or
to journal the previous binding and restore it when
`register_class_decl` restores `current_package`), which touches the same
suppression machinery that several Cro/`Header`-shaped regressions are pinned
on.

## Repro

`tmp/rlc7.p6` (recreate from the snippet above — `tmp/` is gitignored).
`t/role-lexical-class.t` deliberately uses a *different* short name for the
class-nested case so it does not depend on this bug.
