# `.raku` of a cached Seq of class instances renders as a List

`Seq.cache` returns a `List`, and `.raku` already rendered it as `(..)` / `$(..)`
for plain values. When the elements were class instances, the leaf-dispatch
expansion rebuilt the cached view as a real `Seq`, so it printed `(..).Seq`
(or, for a not-yet-pulled gather, an empty `$( )`). The expansion now keeps the
List view. Found with the `annotations` distribution (#15259); fixes #15461.
