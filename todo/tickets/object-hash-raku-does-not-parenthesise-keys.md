# `.raku` on an object hash does not parenthesise its keys

Rakudo renders an object hash's pairs with the key object's `.raku` wrapped in
parentheses; mutsu prints the bare key:

```raku
my class S { }
my %o{Mu};
%o{S} = 7;
say %o.raku;
```

```
raku:  (my Any %{Mu} = (S) => 7)
mutsu: (my Any %{Mu} = S => 7)
```

The `%{KeyType}` part is right (fixed in
`news/2026-08/object-hash-key-type-survives-parameter-binding.md`); only the
per-pair key rendering differs. The parenthesised form is what makes the output
round-trip — `S => 7` would parse as the string key `"S"`.

The hash arm that builds the `(my ValueType %{KeyType} = …)` form is in
`src/builtins/methods_0arg/raku_repr.rs` (search for `map.key_type`), but the
`parts` it joins are built by the generic pair renderer above it, which does not
know it is inside an object hash. The slow-path twin is
`dispatch_constrained_hash_raku`; both need the same treatment, and the fix
should check what Rakudo does for a *string* key in an object hash
(`my %h{Mu}; %h{"a"} = 1`) before parenthesising unconditionally.

Low priority — cosmetic, and no known test depends on it.
