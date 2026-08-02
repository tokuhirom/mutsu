# `.cache` on a genuinely-lazy Seq still answers `Seq` — and that crashes `is-deeply`

`Seq.cache` returns the underlying `List`. mutsu returns a `List` for every
ordinary Seq, but for a *genuinely lazy* one it returns the lazy value unchanged,
whose `.^name` is `Seq`:

```raku
my $cat := IO::CatHandle.new: tmpfile("a1\na2"), tmpfile("b1\nb2");
my $m = $cat.handles.map({ 1 });
say $m.cache.^name;    # raku: List    mutsu: Seq
say $m.List.^name;     # raku: List    mutsu: Seq
```

## Why it is not cosmetic: it aborts the process

The vendored upstream `Test.rakumod` dispatches `is-deeply` through Seq-narrowing
candidates that peel the laziness off by calling `.cache`:

```raku
multi sub is-deeply(Seq:D $got, Seq:D $expected, $reason = '') is export {
    is-deeply $got.cache, $expected.cache, $reason;
}
```

If `$got.cache` is still `Seq:D` the call re-dispatches to the *same* candidate,
forever. Under `MUTSU_REAL_TEST=1`, `t/io-cathandle-lazy.t` does not fail — it
dies with `fatal runtime error: stack overflow, aborting` and dumps core.

Minimal reproduction (needs no `Test`, ~10 lines): build a lazy Seq as above and
call `.cache` twice.

## Two separable gaps

1. **`.cache` / `.List` on a genuinely-lazy list.** `methods_0arg/collection.rs`'s
   `"cache"` arm deliberately keeps a `LazyList` lazy (correct — Rakudo's `.cache`
   reifies on demand, it does not force), but mutsu has no *lazy List* value: a
   `LazyList` reports `Seq`. raku's answer is a lazy `List`, so the type changes
   while the laziness does not. Giving `LazyList` a "this is a List, not a Seq"
   bit — or a distinct lazy-List representation — is the real fix, and it is a
   Value-representation question (PLAN §3 lazy-seq).
2. **`IO::CatHandle.handles` is wrongly lazy, and wrongly an Array.**

   | | raku | mutsu |
   | --- | --- | --- |
   | `$cat.handles.^name` | `Seq` | `Array` |
   | `$cat.handles.is-lazy` | `False` | `True` |

   Fixing this alone would take `t/io-cathandle-lazy.t` out of the crash, but it
   leaves gap 1 for every genuinely-lazy Seq that reaches `is-deeply`.

Do gap 1; gap 2 is a correctness fix worth doing on its own either way.

## Also in that file (separate, not a crash)

`is-deeply $cat.lines, ("a", "b", "c")` reports `got: ("a", "b", "c").Seq` — the
real `is-deeply`'s `eqv` compares a Seq against a List and says no, where raku's
Seq-narrowing candidate has already turned both into Lists. Same root as gap 1.

Found in the vendored-`Test.rakumod` campaign,
`todo/tickets/vendor-real-test-module.md`.
