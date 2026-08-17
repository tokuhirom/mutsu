# Calling an instance attribute accessor on a bare type object now gives the correct error

```raku
class Foo { has $.x; }
say Foo.x;
```

raku:

```
Cannot look up attributes in a Foo type object. Did you forget a '.new'?
  in method x at -e line 1
  in block <unit> at -e line 1
```

mutsu used to say `No such method 'x' for invocant of type 'Foo'` instead.

`Interpreter::should_bypass_native_fastpath`'s `Package` (type-object)
receiver branch deliberately never calls `has_public_accessor` — only
`has_user_method`/`has_class_level_attr` — since an instance attribute's
accessor is meaningless called on the bare type
(`resolve_user_method_or_accessor_would_wrongly_answer_for_a_package_receiver`,
`src/runtime/methods_native_bypass.rs`, ADR-0019 E4b step 11). So for an
accessor-only class with no other method of the same name, the call never
routed to the accessor at all — it fell all the way through the native
`native_method_{0,1,2}arg` cascade to a generic "no such method" error.

raku instead resolves `.x` to the real accessor and lets *that* accessor's
own body raise "Cannot look up attributes..." when it tries to read
attribute storage that doesn't exist on a type object. mutsu already had this
exact error message implemented, but only as a narrow special case for
`.name` (`methods_instance_ops.rs` — `.name` collides with the type-name
introspection method, so it already checked `has_public_accessor` before
falling back to plain type-name stringification).

Generalized that check to the final "no such method" fallback in
`methods_instance_ops.rs`: for any `Package` receiver whose class has a
public accessor matching the called (zero-arg) method name, raise the
"Cannot look up attributes..." error directly instead of falling through to
`make_method_not_found_error`. A private attribute (no public accessor) or an
explicit user method of the same name (which still wins over the accessor)
are both unaffected. Regression test:
`t/package-receiver-accessor-type-object-error.t`.
