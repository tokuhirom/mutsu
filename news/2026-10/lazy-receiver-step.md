# A lazy receiver's methods go through the lazy step

The by-name `LazyList` blocks of the slow-path method dispatcher (the `gist`/`Str`/`raku`
placeholders, `cache`, `AT-POS`/`EXISTS-POS` pulling up to the index, `join`'s `...`, and the
50-name force-and-redispatch list) are now one *lazy step* (ADR-13891). Each method
that reads only part of a lazy list has a *pull*, registered on the owner Rakudo declares the
method on (`Seq`, `List`, `Array`, `Any`) and found through the MRO of the type the list
reports; a method that needs every element reifies the list and is answered by the settled
`Seq`'s rows. Answers are unchanged.
