# A method call decontainerizes an itemized Hash invocant, as it already did for an Array

A method call decontainerizes its invocant, so a method on a `$`-held hash (or an element read out
of an Array/Hash, `.item`, `$(...)`) operates on the hash's Pairs. mutsu kept the holder's
itemization on the receiver, and every method that decomposed its receiver through
`value_to_list(target)` then saw the whole hash as ONE opaque item:

```raku
my $s = {a => 1};
say $s.tail.raku;           # rakudo: :a(1)            mutsu before: ${:a(1)}
say $s.rotor(1).raku;       # rakudo: ((:a(1),),).Seq  mutsu before: ((${:a(1)},),)
say $s.cache.raku;          # rakudo: (:a(1),)         mutsu before: (${:a(1)},)
say $s.permutations.raku;   # rakudo: ((:a(1),),).Seq  mutsu before: ((${:a(1)},),)
my $t = {a => 1, b => 2};
say $t.tail(2).sort.raku;   # rakudo: (:a(1), :b(2)).Seq   mutsu before: (${:a(1), :b(2)},)
```

`Interpreter::call_method_with_values` already re-dispatched an itemized **Array** receiver on its
decontainerized value for every method but `raku`/`perl`/`item`/`self`/`VAR`. Its Hash half only
redirected `.VAR`, on the reasoning that "an itemized Hash is still a Hash" and the renderers and
element-flattening chokepoints already consult `hash_is_itemized`. That holds for those, but not for
a method that decomposes its *receiver*: `value_to_list` answers "does this flatten as an ELEMENT
of another container" (ADR-0040), which is the opposite question.

Two changes, because the interpreter and the native zero-argument path are separate entries:

- `call_method_with_values` gives the Hash half the same decontainerizing re-dispatch the Array
  half has.
- `native_method_0arg` -- the one entry for every zero-argument native method, reached from every
  VM call op as well as the interpreter -- clears the flag on an itemized Hash receiver before it
  dispatches (`.cache`, `.permutations` and the other zero-argument methods are served there). It
  is a tag probe, `hash_is_itemized`, that is false for every non-Hash.

The flag is cleared over the SAME `HashData` `Gc`, so a mutator (`.push`, `.append`, `:delete`) still
writes through and the variable keeps its itemization (`$h.push((b => 2)); $h.raku` stays
`${:a(1), :b(2)}`). `.raku`/`.perl`/`.item`/`.self` still observe the container.

A 70-method sweep over an itemized Hash receiver, compared against `raku`, now differs only where a
bare `%h` differs too: those independent gaps are filed as
[#10758](https://github.com/tokuhirom/mutsu/issues/10758) (`Seq`, `reverse`, `unique`, `squish`,
`eager`, `minmax`, `produce`, `Supply`, `duckmap` on any Hash). The same sweep over an itemized Array
already agreed with `raku`.

Pinned by `t/collections/hash/hash-itemized-receiver-methods.t` (30 assertions, every one also
checked against `raku`). Closes #10744.
