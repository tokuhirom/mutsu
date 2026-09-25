# An operator declared inside `sub EXPORT` is a declared operator to the importer

`Understitch` builds its operator as a plain local routine inside its
`sub EXPORT` hook and hands it out through the returned `Map`:

```raku
sub EXPORT(\stitchwith = " ") {
  sub infix:<_> (Cool $a, Cool $b) is equiv(&infix:<~>) is assoc('left') { ... }
  Map.new: '&infix:<_>' => &infix:<_>,
}
```

Plain infix use (`"abc" _ "def"`) already worked, but only by accident: the
parser's speculative custom-infix-word matcher accepts any non-reserved word as
an infix and resolves it at run time. Every form that asks whether an operator
is *declared* rejected it — above all the reduction metaop, so
`is(([_] 'aa' .. 'bb'), ...)` in `t/10-basics.t` died with this #7988 cluster's
generic `Confused. expected statement: ...` message. The importer's static
module scan only looked for `is export` declarations (at unit scope, and since
Logic::Ternary also inside the hook body); a categorical routine declared in the
hook without the trait was invisible to it.

The scan now also collects every categorical routine (`infix:<...>`,
`prefix:<...>`, `postfix:<...>`, `circumfix:<...>`, `postcircumfix:<...>`)
declared inside the unit-scope `sub EXPORT` body, trait or no trait, with its
precedence and associativity. Such a routine has no purpose other than being
exported: the hook's body is a private lexical scope that ends when it returns.
So `is equiv(&infix:<~>)` now reaches the importer's parse too: `1 _ 2 + 3`
binds as `1 _ (2 + 3)`, as in raku.

Pinned by `t/modules/import-export/export-hook-local-operator-reduction.t`
(fixture `t/lib/ExportHookReduceOp.rakumod`). `Understitch`'s `t/10-basics.t`
now passes; its remaining red file, `t/07-properties.t`, fails under rakudo as
well.
