# An object hash stores its key de-itemized

```raku
my %j{List:D};
my $t = $(1, 2);
%j{$t} = "x";
say %j.keys[0].raku;
# raku:  (1, 2)      mutsu: $(1, 2)
```

`news/2026-09/itemized-hash-subscript-is-one-key.md` landed the key correctly —
one entry, and `%j.keys[0].^name` is `List` — but the key kept its
**itemization tag**, which raku drops when it stores it.

It was purely a store-side normalization gap. The subscript paths deliberately
normalize an itemized hash index to a `Scalar` wrapper: that is the one shape
the slice machinery does not read as a list, and it is what makes read /
assign / `:exists` / `:delete` agree on one key. The wrapper is the right
*transport*; what was missing is the object hash unwrapping it when it records
the key value it will later hand back.

## The fix

`Interpreter::object_hash_key_value` at the three sites that record an object
hash's original key. It applies `deref_container` and `deitemize_element`
**twice**, because the index can arrive as a `Scalar` wrapper around the
variable's own `ContainerRef` cell (`%h{$t}`) and neither of those looks
through the other.

Unwrapping only the *recorded* value is what keeps the round-trip: the `.WHICH`
string the entry is filed under is still computed from the index as given, so
`%h{$t}`, `%h{$t}:exists` and `%h{$t}:delete` all still find the same entry —
pinned, along with the itemized `Array` and `Hash` key spellings.

Pinned by `t/object-hash-key-is-deitemized.t`, whose 14 assertions pass
unchanged under rakudo. `t/itemized-hash-subscript.t` is unmoved.

One divergence in the same measurement matrix turned out to be independent and
is recorded as
`todo/tickets/object-hash-composite-key-increment-does-not-accumulate.md`:
`%p{$p}++` for a composite (`.WHICH`-derived) key does not accumulate, with or
without itemization, while a scalar-keyed object hash increments fine.
