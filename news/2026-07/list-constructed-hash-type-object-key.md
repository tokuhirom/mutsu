# List-constructed hash type-object keys coerce to "" with a warning

A bare type object used as a hash key in *list* construction now stringifies to
the empty string with Rakudo's "Use of uninitialized value of type X in string
context" warning, matching the subscript-store behavior shipped earlier
(`%h{Int} = 1`). Previously mutsu kept the type object's gist as the key:

```raku
my %h = (Int, 1);      # was: keys ((Int));  now: keys ("",)  + warning
say %(Int, 1).keys;    # was: ((Int));       now: ()          + warning
(Int, 1).hash;         # was: {(Int) => 1};  now: {"" => 1}    + warning
```

This holds across every list-to-hash path: the `my %h = (...)` assignment, the
`%(...)` literal, the `.hash`/`.Hash` methods, and the `hash(...)`/`Hash(...)`
coercers. A class defining a user `.Str`/`.Stringy` dispatches it instead of
warning (`(Foo, 1).hash` keys by `Foo.Str`), and ordinary string/numeric keys
are unaffected.

## Implementation

The pairing logic in `build_hash_from_items` was factored into
`build_hash_from_items_with_key_coercion`, which takes a per-key encoder hook.
The interpreter-aware `Interpreter::build_hash_from_items_warning` uses it to
coerce a bare type-object key via the existing `coerce_type_object_hash_key`
helper (warn + optional user `.Str` dispatch), while the plain
`build_hash_from_items` keeps the original gist encoding and `original_keys`
recording.

The empty-string coercion is a *plain* (`Str`-keyed) hash semantic only. An
object hash (`%h{Any}`, `%h{Int}`, ...) keeps its keys distinct — collapsing
them to `""` would drop the key entirely — so `coerce_hash_var_value` gates the
warning variant behind `is_str_keyed_hash_var` and routes object hashes through
the unchanged `build_hash_from_items`, which preserves the type-object key for
the object-hash re-tag and for `.antipairs`/`.invert`.

The `.hash`/`.Hash` methods (and the `%(...)` literal, which compiles to
`MakeArray(...).hash`) are dispatched natively without an interpreter, so a
list invocant is intercepted in the VM `CallMethod` path (and in
`call_method_with_values` for re-entrant dispatch) and routed to the
interpreter-aware builder. `exec_make_hash_op` now coerces its type-object keys
too.

Pinned by `t/hash-key-type-object-list.t`.
