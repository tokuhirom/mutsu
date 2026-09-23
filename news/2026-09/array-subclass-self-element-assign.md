# `$obj.self[1] = 1` on an `is Array` instance stores into the instance

`class A is Array {}; my $a = A.new; $a.self[1] = 1` died with "No matching
candidates for method: self" (Rakudo: `[(Any) 1]`).

Two gaps met on this line.

- **`.self` was delegated to the backing storage.** A method an `is Array`
  subclass does not define is answered by its `__mutsu_array_storage`, and
  `delegates_to_array_storage` exempts only the type-identity and
  construction methods. `self` was not on that list, so the interpreter's
  `call_method_with_values` handed back the storage `Array` instead of the
  instance. `Mu.self` answers the invocant itself; it is now on the exempt
  list next to `WHAT`/`WHICH`.
- **The accessor-lvalue store had no arm for an `is Array`/`is Hash`
  object.** `X.method[i] = v` compiles to `__mutsu_index_assign_method_lvalue`,
  which calls the accessor, copies the container it returns, and writes the
  copy back by calling the accessor as a setter. When the accessor returns an
  `is Array`/`is Hash` instance there is nothing to write back — the element
  lives in the instance's storage attribute, shared by every alias — and a
  method with no setter (`.self`) died, while a read-only attribute accessor
  (`has $.arr = A.new; $c.arr[0] = 7`) silently dropped the store. The new
  `store_into_storage_instance_element` stores into the storage in place,
  the same rule the computed-target op already applied to this shape.

Pinned by `t/collections/array/array-subclass-self-element-assign.t` (#9168).
The per-store copy of the `is Array` storage is the existing cost tracked in
#9157.
