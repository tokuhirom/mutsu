# A Buf/Blob answers the Associative protocol error, not "Index out of range"

```raku
my $buf = Buf.new(1, 2, 3);
$buf<a> = 5;
```

`raku` says:

```
Type Buf does not support associative indexing.
```

mutsu said:

```
Index out of range
```

`Buf`/`Blob` do `Positional`, not `Associative`, so rakudo refuses an
associative subscript store (`$buf<a> = 5`, `$buf{0} = 5`) before it ever
reaches an element store — regardless of whether the key happens to look
numeric. mutsu's keyed-store path reaches a `Buf`/`Blob` as an `Instance`
carrying a native storage attribute rather than one of the ordinary
`ValueView` array/hash shapes, so it fell into the same code that serves
positional stores, tried to coerce the key with `index_to_usize`, and
reported "Index out of range" when that failed (or silently wrote to the
wrong slot when the key happened to parse as one, e.g. `$buf{0} = 5`).

Fixed by refusing an associative subscript store on this instance shape up
front, mirroring the Positional/Associative protocol error already used for
the `ValueView`-based receivers (`Seq`, `List`, `Array`, `Range`, `Int`,
`Str`) in `scalar_subscript_protocol_error`. `Blob`'s existing
read-only refusal for a *positional* store is untouched; the associative
check is a separate, earlier branch since neither type does Associative at
all.

`t/collections/subscript/scalar-subscript-protocol.t` gained 3 assertions
covering `Buf`'s associative refusal (`<>` and `{}` spellings, and a
numeric-looking key) alongside the existing Positional/Associative rows for
other types.

Part of the survey in
[#7556](https://github.com/tokuhirom/mutsu/issues/7556).
