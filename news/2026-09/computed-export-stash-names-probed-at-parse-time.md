# Operators a module names at run time are known to the importer's parse

Moneys exports one postfix operator per currency code, generating the names in
a loop over a `constant` hash:

```raku
my package EXPORT::ALL {
    for %currencies.keys -> $code {
        OUR::{'&postfix:<' ~ $code ~ '>'} := sub (Rat:D $amount) { ... };
    }
}
```

Rakudo loads a module while it compiles the `use`, so `50.0CAD` in the
importer parses as a postfix call. mutsu learns imports from a static scan of
the module source, which cannot see a name the module computes, and its test
died at parse time (#9500).

The scan now flags a module that binds into its own export stash under a
non-literal key. A `use` of such a module runs it at parse time in a fresh
interpreter on its own thread — the path slang activation already uses — and
registers every routine the load exported with the parser. The literal-key
form stays the static scan's job (#9499). The decision, its gate and its
trade-offs (the module's mainline runs twice) are
[ADR-0124](../../docs/adr/0124-parse-time-export-probe-for-computed-export-stashes.md).

Two bugs behind the next Moneys line were fixed along the way. A user
`prefix:<->` never matched `-Money.new(...)`: the matcher counted `-` as an
identifier character and demanded a word boundary after the operator. And
when a user `multi prefix:<->` made `-$x` a call that none of its candidates
took, the core fallback negated the lvalue wrapper instead of its value and
answered `0`. Moneys' suite now passes 16/16.
