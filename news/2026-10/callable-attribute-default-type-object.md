# An unset `&` attribute holds the `Callable` type object

```raku
my sub is-set(&c --> Bool:D) { return False without &c; True }
class K { has &.cancelled; method probe { is-set(&!cancelled) } }
K.new.probe;   # raku: False; mutsu: "expected Callable but got Any"
```

An untyped `&`-sigil attribute with no initializer was seeded with the `Any` type
object (the `$`-sigil default), which a `&` parameter rejects. It now holds
`Callable`, in all three places that seed attributes (`bless`, `CREATE` and the
cached construction plan). `MCP::Client`'s `Leases` passes `&!cancelled` straight
to a `&cancelled` parameter on every contended acquire, so its `t/20-leases`
waiting tests failed.
