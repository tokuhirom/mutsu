# `class C is List { }` takes a positional `.new`

`class C is Array { }` inherited `Array.new`'s positional constructor and
answered positional methods from the instance's backing
`__mutsu_array_storage`. `class C is List { }` did neither — it fell through to
`Mu.new` and died:

```raku
class MyList is List { }
MyList.new('a', 'b');   # Default constructor for 'MyList' only takes named arguments
```

Every gate that recognised the backing store tested the MRO for `Array` alone.
They now go through `Interpreter::is_positional_base`, which accepts `Array` and
`List`: construction seeds the storage from the positional arguments, and
`.elems` / `.join` / `AT-POS` / iteration delegate to it.

The two bases keep their different mutability. A fresh instance's storage kind is
chosen by `positional_base_storage`: an `Array` subclass gets a real `Array`, a
`List` subclass an immutable `List`, so `.push` on the latter still raises
`X::Immutable` exactly as raku does.

## Result

`Cro::HTTP::MultiValue is List does Stringy` — the type Cro uses whenever a query
string or a form body repeats a key — is constructible and stringifies
correctly. In `t/http-request-parser.rakutest` the "Query strings with multiple
values for the same key" and "Multiple entries with same name in
application/x-www-form-urlencoded" cases now pass.

The remaining multi-value assertion (`%hash<a>[*]`) is blocked on a separate
gap — a whatever-slice reads nothing from *any* storage-backed instance,
including an `is Array` one — recorded in
`todo/tickets/whatever-slice-on-a-storage-backed-instance-gives-nil.md`.

Pinned by `t/is-list-subclass.t`.
