# A `&trait_mod:<is>` dispatcher returned by `sub EXPORT` applies its traits

Upstream NativeCall installs `is native` this way:

```raku
sub EXPORT(|) {
    my $native_trait := multi trait_mod:<is>(Routine $r, :$native!) { ... };
    Map.new('&trait_mod:<is>' => $native_trait.dispatcher);
}
```

That `multi` is lexical to `EXPORT`. mutsu's `.dispatcher` used to return a
handle that looked the routine up by name, so the importer received a
dispatcher with no candidates. A `sub f() is native` there was silently left
alone unless some other `is export` candidate happened to be in scope.

`.dispatcher` on a multi candidate now captures its candidates, the way
`&name` of a multi does. Routine and parameter trait application first tries
the `&trait_mod:<is>` a `sub EXPORT` installed, and falls back to the by-name
dispatch when none of its candidates accepts the trait. As a side effect, a
re-exported operator dispatcher (`'&infix:<op>' => $t.dispatcher`) no longer
recurses until the stack overflows (#11530).
