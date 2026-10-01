# Type-check failures name the offending object's class and its `.raku`

`class C { has R $.r }; C.new(:r(F.new))` died with `expected R but got Any`; Rakudo says
`expected R but got F (F.new)`. The same wrong `Any` (and a missing `(repr)`) appeared for every
object a type check rejected, which made a zef plugin-loader failure read as "a defined
non-Fetcher object is `Any`" ([#10248](https://github.com/tokuhirom/mutsu/issues/10248)).

Two things were wrong. `value_type_name` answers the generic `Any` for every `Instance`, so the
`got` half never named the class. And `value_short_repr`, the pure function behind the `(repr)`
half, had no answer for an object: an object's repr is its `.raku`, which a class may override,
so it is a method call and the interpreter-free error constructors cannot make it.

- `got_type_name` now names an instance's class (`Outer::Inner`, lexical `my class` names
  demangled), shared by every `X::TypeCheck::*` message.
- The builders in `runtime/utils/type_check_errors.rs` (moved out of `errors.rs`) take the
  `(repr)` text as a parameter, and `Interpreter::type_check_got_repr`
  (`runtime/type_check_repr.rs`) renders it: a user-declared `raku` runs through compiled method
  dispatch (`try_dispatch_compiled_method_direct`), a class without one gets the built-in
  default renderer (`default_instance_repr`, the one `.raku` itself reaches) -- no
  `call_method_with_values` fallback. A `raku` that dies falls back to the default text, because
  the message must report the type mismatch, not that.
- Rakudo cuts a long repr to its first 20 characters plus `...` (`got Str ("aaaaaaaaaaaaaaaaaaa...)`,
  `Wide.new(text => "bb...)`), objects and strings alike; `short_repr_of_raku` does the same and
  now applies to every value, not only objects.
- Every assignment, element-store, `:=` binding and routine-parameter-binding site
  (`X::TypeCheck::Assignment`/`Binding`/`Binding::Parameter`) goes through the interpreter-aware
  wrappers. `t/types/coercion/typecheck-got-object-repr.t` runs identically under `raku`.

Not covered, filed separately: collections (`List`/`Array`/`Hash`) still print no `(repr)` tail
([#10639](https://github.com/tokuhirom/mutsu/issues/10639)), and the plain-`sub` call path words
its own mismatch (`Calling f(Any) will never work ...`, `expected Int, got Any`;
[#10640](https://github.com/tokuhirom/mutsu/issues/10640)).
