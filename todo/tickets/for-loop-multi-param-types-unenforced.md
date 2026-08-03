# A multi-parameter `for` loop does not enforce its parameter types

```raku
for ("a", 1, "b", "two") -> Str $k, Int $v { say "$k=$v" }
# raku:  a=1
#        Type check failed in binding to parameter '$v'; expected Int but got Str ("two")
# mutsu: a=1
#        b=two
```

Declared types on the parameters of a multi-parameter pointy block are accepted
by the parser and then ignored at run time. The single-parameter form
(`for @a -> Int $x`) is unaffected — only the multi-param path.

## Where it is

`ForLoopSpec` (`src/opcode.rs`) carries `multi_param_names: Vec<String>` and no
per-parameter constraint, so the VM has nothing to check against. The compiler
*does* have them: `params_def: &[crate::ast::ParamDef]` is in scope where the
`ForLoop` opcode is emitted (`src/compiler/stmt.rs`, the `multi_param_names`
field), each with its `type_constraint`.

The bindings themselves are plain `Stmt::Assign`s built by
`build_for_bind_stmts` (`src/compiler/mod.rs`), which drop the constraint
entirely.

## Shape of the fix

Add a parallel `multi_param_type_constraints: Vec<Option<String>>` to
`ForLoopSpec`, populate it from `params_def`, and have `exec_for_loop_body` call
`bind_param_type_constraint(name, tc)` per parameter instead of the blanket clear
introduced by `news/2026-08/for-multi-param-stale-type-constraint.md` (which
clears the stale name-keyed constraint and restores it after the loop). That
gives both the shadowing semantics and the enforcement in one place.

Two things to get right:

- The error must be a *binding* failure (`X::TypeCheck::Binding::Parameter`,
  "Type check failed in binding to parameter '$v'"), not the assignment error the
  `SetLocal` path would produce.
- Coercion types (`Str(Cool) $v`) and `is rw` / sigilless (`\v`) parameters must
  keep working; the clear-and-restore currently applies to every name in
  `multi_param_names`.

Keep `size_of::<OpCode>() <= 48` in mind — `ForLoopSpec` is already boxed, so a
new `Vec` field is free there.
