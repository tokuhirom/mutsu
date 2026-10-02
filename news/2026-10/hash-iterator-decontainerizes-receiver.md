# `.iterator` on an itemized Hash iterates its Pairs

A method call decontainerizes its invocant, so `.iterator` on a `$`-held hash (or an element read
out of an Array/Hash, `.item`, `$(...)`) iterates the hash's Pairs. mutsu answered the whole Hash
as one opaque element:

```raku
my $s = {a => 1};
say $s.iterator.pull-one.^name;      # rakudo: Pair    mutsu before: Hash
say $s.iterator.pull-one.raku;       # rakudo: :a(1)   mutsu before: ${:a(1)}
my %h = a => 1;
say $(%h).iterator.pull-one.^name;   # rakudo: Pair    mutsu before: Hash
say %h.iterator.pull-one.^name;      # Pair in both (a bare hash was already fine)
```

A Hash's itemization is a flag on the value (the same `HashData`, mirroring `ArrayKind`'s
`ItemArray`), so an itemized receiver reaches the `iterator` builder still carrying it.
`build_iterator_instance` fell through to `value_to_list`, which answers "does this flatten as an
ELEMENT of another container" and so keeps an itemized hash whole; the itemized-Array case
(`$[1,2,3].iterator`) already had its own arm for exactly this reason, the Hash one was missing.
The fall-through now uses `value_to_list_for_receiver`, the receiver-decomposition twin ADR-0040 §8
introduced for `.pick`/`.roll`/`.head`/`.tail`, which ignores the receiver's own itemization. That
also covers a `Map` held in a `$` (`Map.new((a => 1)).iterator.pull-one` was `$(Map.new(...))`) and
an empty itemized hash. A Uni, a Buf, a Match, a Range, a Seq and a plain scalar iterate exactly as
before.

Pinned by `t/collections/hash/hash-iterator-itemized-receiver.t` (21 assertions, every one also
checked against `raku`). Closes #10661.
