# Returning from a routine no longer copies type names or package variables back

```raku
sub body { my class K { $GLOBAL::k1++ } }
body(); body();
say $GLOBAL::k1;   # 2
```

After the ordinary lexicals (slice 37), a call's return now also stops
copying back type names, package-qualified variables (`$GLOBAL::x`,
`$Foo::v`) and uppercase-named scalars (`my $F`). A package-qualified
variable is now read from the package store first, so a value written by
one routine is what the next one sees, and a class declared with `my` inside
a routine stays that routine's (ADR-12529 §3, slice 38).
