# A List of a `.map`/`.grep` Seq reifies only as far as it is read

rakudo's `.list`, `.cache`, `@$seq` and `@.method` turn a `Seq` into a
`List` that reifies lazily. `@.sources.head` runs a `grep` callback up to
the first match and no further. mutsu already returned such a List view
for `.cache` and `.List` (ADR-0038), and for a `gather`. A not-yet-run
`.map`/`.grep` behaved differently: `@$s` / `@.meth` took the whole source
up front, and `.head` / `.first` on the view reified every element.

Now the `@`-contextualizer and `.list` return the shared List view for a
`.map`/`.grep` pipeline too. `.head(n)` / `.first` on a cached view extend
its prefix with `reify_map_grep_prefix` and keep the rest of the source.
The view still answers `.is-lazy` False, `.elems` and `.WHAT` like a List.

CSS::Font::Resources depends on this: its
`@.sources(|c).head // Source` must not evaluate the trailing fallback
`Source::Local`, whose `.format` looks the font up through the
uninstalled FontConfig module. The test is
`t/collections/lazy-seq/seq-list-view-lazy-prefix.t` (#17773).
