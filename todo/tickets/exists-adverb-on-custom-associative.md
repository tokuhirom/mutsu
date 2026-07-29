# `:exists` on a custom Associative does not dispatch EXISTS-KEY

`$obj<k>:exists` on an instance of a user class/role that `does Associative`
(with `handles <AT-KEY EXISTS-KEY>` or its own `EXISTS-KEY` method) does not
route through the object's `EXISTS-KEY`; it returns False even when a direct
`$obj.EXISTS-KEY('k')` is True. Rakudo dispatches the adverbed subscript to
the method.

Found while pinning DBIish's `TypeConverter` (`has %.Converter is
DBDish::TypeConverter` — the role stores conversions in a typed private hash
and delegates AT-KEY/EXISTS-KEY). The pin test
`t/bless-is-type-container-attr.t` uses the direct `.EXISTS-KEY` call for now;
switch it back to `:exists` when this is fixed.

Repro:

```raku
role TC does Associative {
    has %!store handles <AT-KEY EXISTS-KEY>;
    method STORE(\v) { %!store{.key} = .value for @(v) }
}
class C { has %.conv is TC; submethod BUILD { %!conv = (a => 1) } }
say C.new.conv<a>:exists;         # raku: True, mutsu: False
say C.new.conv.EXISTS-KEY('a');   # both: True
```

Likely a subscript-adverb path that special-cases Hash and falls back to a
plain-hash probe for instances instead of method dispatch.
