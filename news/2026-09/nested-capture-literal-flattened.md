# A nested Capture literal kept its nesting

```raku
say \(1, \(2, 3)).raku;   # raku: \(1, \(2, 3))   mutsu: \(1, 2, 3)
```

`OpCode::MakeCapture` flattened every `Capture` element it was handed, on the
comment's stated theory that such an element "came from a `|capture` slip". It
could not have: `|EXPR` compiles to `MakeSlip`, so an interpolated capture
arrives as a `Slip` and is handled by the arm below it. The only Captures
reaching the flattening arm were genuine *nested literals* — which Raku nests
like any other value.

Found while bundling the `XML` battery. `XML::Element!craft-new` branches on
`$what ~~ Capture` to recurse into a child element, so a nested literal is how
you write a tree in one expression:

```raku
say ~make-xml('rss', :version<2.0>, \('channel', \('title', 'mutsu')));
# raku : <rss version="2.0"><channel><title>mutsu</title></channel></rss>
# mutsu: <rss version="2.0"><channel>title mutsu</channel></rss>   (before)
```

The inner capture was dissolved into the outer one before `craft-new` ever saw
it, so the whole subtree collapsed into text on the parent.

This is the *building* counterpart of
`news/2026-09/slip-array-element-capture-not-respread.md`, which fixed the same
mistaken assumption on the *consuming* side (`append_slip_item` re-spreading a
Capture that merely sat in a slipped array). Both arms assumed "a Capture here
must be a slip"; neither could be, because `|` has its own opcode.

Pin: `t/nested-capture-literal.t` (10 subtests, passes under `raku` too) —
covering two and three levels of nesting, `|$capture` still flattening (both
lanes), a nested capture surviving a slurpy and a re-splatted slurpy, and
`~~ Capture` matching the element (the predicate `craft-new` uses).
