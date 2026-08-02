# `.cache` makes a lazy IO-lines `Seq` repeatable

`.cache` is the one method that makes a `Seq` *repeatable* in rakudo: after it,
the Seq serves its cached elements instead of throwing "The iterator of this Seq
is already in use/consumed by another Seq". Measured, `.list` and `.List` do
**not** — they consume like any other method — so the contract is narrower than
it looks.

mutsu's lazy IO-lines value (`ValueView::LazyIoLines`, what `$path.lines` /
`$path.words` / `words()` over `$*ARGFILES` return) records only a `consumed`
flag. The mut-path method opcode forced it, used the reified `Seq` for the call,
and dropped it — leaving the *receiver variable* holding a spent value. So the
very next method call on the same variable died:

```raku
my $words = $file.words;
$words.cache;
say $words.List;    # The iterator of this Seq is already in use/consumed …
```

`.cache` now stores the reified `Seq` back over the named receiver, which is
exactly the caching contract. Every other method deliberately does not write
back: a Seq consumed by `.map` really is spent.

The consumer is `Test::Util`'s `is-eqv`, whose `Seq:D, Seq:D` candidate opens
with `$got.cache; $expected.cache;` and only then compares — so under the real
module `roast/S16-io/words.t`'s `words() without args uses $*ARGFILES`
assertion died before it could compare anything.

Pin: `t/seq-cache-makes-io-lines-repeatable.t`, verified under both
implementations.

One related laxity is *not* fixed here and is worth knowing about: mutsu does not
throw when a Seq consumed by a non-caching method (`$s.map(...)` then `$s.List`)
is reused, where rakudo does. That is a missing error, not a wrong result, and it
is independent of the caching contract above.
