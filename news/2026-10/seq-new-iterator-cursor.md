# `Seq.new($iterator)` pulls from the iterator's cursor

`Seq.new` had a shortcut for mutsu's built-in `Iterator`: it copied the
instance's whole `items` array into an eager Seq. It ignored the cursor, so
elements an earlier `pull-one` or `skip-one` had consumed came back.
`my $i = (1,2,3).iterator; $i.pull-one; say Seq.new($i)` printed `(1 2 3)`
where Rakudo prints `(2 3)` (#10845). The shortcut is gone. Every iterator now
goes through the deferred Seq that pulls on demand, so the Seq starts at the
cursor and shares the iterator, as Rakudo's does.

Removing the shortcut exposed two gaps in how a deferred iterator-backed Seq
renders:

- `say` and `note` used the pure renderer. It saw the not-yet-pulled body's
  empty seed and printed `()`. This happened even for a user `does Iterator`
  class: `say Seq.new(I.new)` printed `()` while `.gist` was correct. Output
  now routes such a Seq through `.gist` dispatch, which pulls it.
- A Seq over a lazy built-in iterator is now marked lazy, so its `is-lazy` is
  `True`. A lazy, not-yet-pulled iterator Seq gists as Rakudo's `(...)`
  placeholder instead of pulling forever. This covers `Seq.new((1..*).iterator)`
  and `Seq.from-loop({ ... })`, whose `.gist` used to hang.
