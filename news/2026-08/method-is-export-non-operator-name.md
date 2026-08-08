# `method name() is export` (a plain, non-operator name) now works

`class_body_method_decl` and `augment_class`'s method arm handled `is
export` on a method in two different ways depending on the method's name:

- An *operator-categorical* name (`prefix:<~>`, `infix:<as>`, ...) went
  through `register_exported_operator_method_sub`, which builds and
  registers a real `FunctionDef` sub form (self as the first positional,
  forwarding to the method) that `import_module`'s sub-export loop can find
  and copy. This path worked correctly.
- Any other (plain) method name instead called `register_exported_var`,
  which only recorded the name as *exported* — it never built a
  corresponding sub value, so `import_module`'s var-export loop found
  nothing to copy and silently imported nothing:

  ```raku
  class Foo {
      method greet() is export { "hi" }
  }
  import Foo;
  say greet(Foo.new);   # raku: hi -- mutsu: Unknown function: greet
  ```

`register_exported_operator_method_sub`'s forwarding-sub body turned out to
already be name-agnostic despite its name — it dispatches on whatever method
name it's given, operator or not. The fix drops the name-based branch
entirely and always builds the real sub form, at both call sites (the class
walker and `augment_class`, added by ADR-0019 D3-6). The now-unused
`is_operator_categorical_name` helper in `registration_class_body_method_forms.rs`
was removed (a separate copy in the parser, `package_decl.rs`, is unrelated
and stays).

The role walker still has no `is export`/`trait_mod:<is>` handling on
methods at all (a separate, pre-existing gap noted at ADR-0019 D3-3, not
touched here).

Verified against `raku`. Full `t/` suite and the `S12`/`S14` roast whitelist
set all green.
