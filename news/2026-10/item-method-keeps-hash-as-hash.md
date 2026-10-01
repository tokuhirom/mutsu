# `.item` / `$(%h)` keeps a hash a hash, so writes through it are no longer lost

Assigning into an itemized hash was silently dropped (or, worse, replaced the whole hash with a
fresh `{k => v}`) whenever the hash had come out of `.item` or `$( ... )`:

```raku
my %hh = id => 1;
my $h = $(%hh);        # or %hh.item, or ${ ... }
$h<a> = 1;             # rakudo: ${:a(1), :id(1)}   mutsu before: {:a(1)}, %hh untouched
my @c = $(%hh),;
@c[0]<a> = 1;          # likewise
for @c -> $r { $r<out> = 'Y' }   # likewise -- the JSON::RPC shape after JSON::Tiny's from-json
```

The cause was the method form of `.item`. Since the itemization of a `Hash` became a flag on the
value (the same `HashData` `Gc`, mirroring `ArrayKind`'s `ItemArray`), `Value::item()` records it
that way, and `my $h = %hh` already did. But the `item` method arm in
`src/builtins/methods_0arg/dispatch_core_math.rs` still wrapped a hash in a `Scalar` box, which
the subscript-assign lanes do not recognise as a hash. They treated the variable as "not a hash
yet" and replaced it. The arm now delegates to `Value::item()`, so there is one place that decides
how a value records its container, and an array, a hash and a slip all behave the same way.

A method call decontainerizes its invocant in rakudo, and `Hash.Hash` is the hash itself, so
`.Hash` on a `$`-held hash now returns it without the `$` (`my $s = %h; $s.Hash.raku` is
`{:a(1)}`, as `.hash` already did). `[$y, $y].raku` for a `$y` that held `%h.item` also now matches
rakudo, because the element is a plain itemized hash instead of a `Scalar` around one.

Pinned by `t/collections/itemized-hash-write-through.t` (32 assertions, every one also
checked against `raku`). Closes #10601.
