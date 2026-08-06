# `.raku` on an object hash parenthesises type-object and Pair keys

```raku
my class S { }
my %o{Mu};
%o{S} = 7;
say %o.raku;   # raku: (my Any %{Mu} = (S) => 7) — mutsu printed S => 7
```

The parenthesised form is what makes the output round-trip — `S => 7`
would re-parse as the string key `"S"`.

Rakudo's `Hash::Object.raku` just maps `.raku` over its pairs, so the rule
lives in `Pair.raku`: a **non-concrete key** renders as
`(TypeName) => value` and a **Pair key** as `(key.raku) => value`; every
other concrete key is its plain `.raku` (Str keys keep the
colonpair/quoted forms, `42 => 2` and `Bool::True => 5` stay bare).
mutsu's standalone Pair renderer already did this — only the object-hash
pair renderers printed the bare key.

Both now share a new `object_hash_key_repr` helper
(`src/builtins/methods_0arg/raku_repr.rs`) that parenthesises
Package/ParametricRole (type objects, including roles) and Pair keys: the
native fast path and the `dispatch_constrained_hash_raku` slow-path twin
(`src/runtime/methods_native_bypass.rs`, both the typed-hash and `Map`
arms).

Residual found while testing, filed as
`todo/tickets/object-hash-instance-key-raku-rendering.md`: an *instance*
key renders as `U()` instead of `U.new` (the object-hash key path
reconstructs the key from its `.WHICH` and bypasses instance `.raku`
dispatch) — a separate, also-cosmetic gap.

Pinned by `t/object-hash-raku-key-parens.t` (6 cases, verified against
raku).
