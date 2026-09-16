# `for $/<name> { ... }` now flattens a quantified named capture

`for $m<name> { ... }`, where `<name>` is a grammar/regex quantified named
capture (`<name>*`), collapsed into a single iteration over the whole capture
array instead of one iteration per match:

```raku
grammar G {
    token TOP { <a>* }
    token a { 'a' \d+ }
}
my $m = G.parse("a1a2a3");
for $m<a> { .say }
# rakudo: ｢a1｣ ｢a2｣ ｢a3｣ (three iterations)
# mutsu:  one iteration, $_ the whole 3-element Array
```

`desugar_for_scalar_element_source` rewrites any var-rooted `Index` iterable
(`%h<k>` / `@a[i]` / `$m<name>`) into `my $tmp = <ELEM>; for $tmp { ... };
<ELEM> = $tmp;`, topicalizing it as a single rw-aliased element — correct for
an ordinary Hash/Array element, whose value always lives in a Scalar
container (itemized on insert). A Match's capture-map values are
deliberately "bare" (non-itemized) instead, per `Value::hash_bare_values`'s
own doc comment (`$/.hash<x>.VAR.^name` is `Array`, not `Scalar`) — a
quantified capture's value is a genuine flattening `List`/`Array`. Plain
assignment into `$tmp` itemizes it regardless (`my $x = EXPR` always does in
Raku), so the rewrite silently turned a multi-match capture into one item.

The fix extends the desugaring's existing runtime-guarded slice/element
decision — already used for a subscript whose shape might be a Range/list at
runtime — to also check the read element's own itemization via `<ELEM>.VAR ~~
Scalar`, alongside the index-shape check. A non-scalar (non-itemized) element
takes the same flatten-without-writeback path a slice does, matching
Rakudo's real per-container itemization rule instead of assuming every
var-rooted index read is a single itemized value. The ordinary Hash/Array
single-element rw-aliasing writeback this desugaring exists for (`for @a[i]
{ .=uc }` mutating `@a[i]`) is unaffected.

Found re-measuring `ecosystem/dists/A/ANTLR4--Grammar~27d2b464.json`: its
Perl6 action class's `TOP` method relies on `for $/<prequelConstruct> { ... }`
flattening a quantified capture, and every roast/local test exercising a `for`
over such a capture hit this. `ANTLR4::Grammar` moves from `red` (0/13 files)
to `partial` (1/13, `t/meta.t` now green); its remaining red files hit a
separate, pre-existing multi-method dispatch specificity bug tracked as
[#8566](https://github.com/tokuhirom/mutsu/issues/8566).

Pinned by `t/grammar/grammar-quantified-capture-for-loop.t`.
