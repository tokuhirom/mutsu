# Trie: nested gathers are read, and a Seq prefers `@` over a coercion

#11682 took Trie's `t/02-trie` to 67/70. This change fixes the last three
failures, so the file now passes 70/70, matching rakudo.

- **A finite `gather` nested in a list is pulled when the list is read.** This
  covers a gather reached through a variable, a gather that is a Pair's value,
  and a gather returned by a hyper method call (`@nodes>>.get-all`). Such a
  gather used to read as empty. It now renders its values in `say`, `.gist`
  and `.Str`, and in the `:p`/`:kv` adverb rows. Two gaps from #11678 remain:
  the element is not yet kept as a single `Seq`, and `.raku` still flattens it.
- **A Seq argument prefers `@arr` over a `Str()` coercion candidate** in
  method dispatch. `"ab".comb` is passed to `multi method d(@arr)` rather than
  to `d(Str() $k)`. `t/oo/method/method-seq-arg-positional-over-coercion.t`
  pins this.
