# A role's attributes keep their declared types, and an attribute object hash keys by `.WHICH`

Two constraints that a `has` declaration writes down were being thrown away
before anything could enforce them. Both surfaced while reducing `DBIish`'s
`t/06-types.rakutest`, whose `role TypeConverter` declares
`has Callable %!Conversions{Mu:U}` — a single attribute that needs each of them.

## A role attribute had no declared type at all

`register_role_decl` destructured `Stmt::HasDecl` with `type_constraint: _` and
`type_smiley: _`. A role has no `ClassDef` of its own to hold `attribute_types`,
and nothing else recorded them, so `role R { has Int $.x }` accepted anything:

```raku
role R { has Int $.x; has Int @.a }
class C does R { }
C.new(:x('no'));      # raku: type check failure     mutsu: accepted
C.new.a.push('str');  # raku: type check failure     mutsu: accepted
C.new.a.of;           # raku: Int                    mutsu: Mu
```

Roles now record the constraint (and the definiteness smiley) per
`(role, attribute)`, in the same `ValueType{KeyType}` encoding a class attribute
uses, following the pattern `role_attribute_is_types` already established for
the `is Type` container trait. At composition the entries are copied onto the
consuming class's `attribute_types` — with `::?CLASS` resolved to that class and
any role type parameter substituted, so `role P[::T] { has T $.v }` composed with
`Int` constrains `$!v` to `Int`. `ensure_role_punned_to_class` carries them too
(there `::?CLASS` is the role itself), along with the role's `wildcard_handles`,
which the punned class had also been dropping.

The punned construction path builds its instance directly instead of going
through the class constructor, so it grew the same type check the class path
does in `enforce_attribute_where_constraints`. Punned roles now run their own
`new`, and their default constructor rejects positional arguments like an
ordinary class.

## An object hash declared as an attribute stringified its keys

`has %!c{Mu:U}` parsed correctly and the container really did carry the key
constraint — `.raku` printed `my Callable %{Mu:U}` — but every element
assignment stored under the *stringified* key, so `%!c{Str}` and `%!c{Int}` both
landed on `""` (with a "Use of uninitialized value of type Str in string
context" warning to match). A lexical `my %h{Mu:U}` was correct all along.

The reason is that the ~10 element-access sites ask
`var_hash_key_constraint(name)`, which is keyed by variable name — and an
attribute's declared type lives in the class registry, not in the per-variable
map, exactly as `scalar_attr_type_constraint` already documents for typed scalar
attributes. That lookup now falls back to resolving `%!c` / `%.c` against the
current `self`'s class and splitting the key part back out of the declared type,
so every path that consults it — assignment, read, `:exists`, `:delete` — sees
the constraint. `var_hash_key_constraint_fast`, which gates a fast element-assign
path that cannot handle object hashes, resolves it too; that costs a class-registry
probe only for `%`-sigil attribute names, which never appear on the hot
local-variable path.

Pinned by `t/role-attribute-type-constraint.t`.
