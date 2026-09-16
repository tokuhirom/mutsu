# A `--> Nil` method didn't sink its tail `.map`, and `.=new` on a populated QuantHash died

Working `Game::Entities` 0.1.6's own `t/entities.t` (issue #8496) surfaced two independent,
general-purpose bugs.

## A method's own tail wasn't sunk

`sub f(--> Nil) { (1,2,3).map: { say $_ } }` already ran its callback under mutsu: a `sub` whose
signature fixes the return value compiles its body with `sink_last_expr`, which routes the tail
expression through the normal statement-sink machinery (`OpCode::SinkPop`) before substituting the
fixed value. The identical `method f(--> Nil) { ... }` did not: `compile_method_body` is a
completely separate compile path from `compile_sub_body` (ADR-0019 D3-8a) and never consulted
`return_type` at all, so a method's tail expression was left on the stack as an ordinary implicit
return with no sink.

The runtime *did* have a fallback for this — `Interpreter::sink_for_definite_return`, called from
`finalize_return_with_spec` for both subs and methods — but it only knew how to drain a `LazyList`
or a value whose elements were already reified via `Deref`. ADR-0058 made `.map`/`.grep` return a
`Seq` that is deliberately **not** reified until first consumption (`SeqSource::MapGrep`), so its
`Deref` read returns the empty seed and the fallback silently iterated zero elements — the callback
never ran, and no error surfaced either.

`Game::Entities`'s multi-component delete is exactly this shape:

```raku
multi method delete ( $guid, *@c where *.elems > 1 --> Nil ) {
    @c.map: { $.delete: $guid, $_ }
}
```

so the issue's own reduction deleted nothing at all:

```raku
$e.delete: $id, Named, Aging, Other;
say $e.check($id, Other);   # mutsu: True (nothing was deleted); raku: False
```

Fixed by teaching `sink_for_definite_return` the same `ValueView::Seq(body) if body.needs_touch()`
case `OpCode::SinkPop` already has, draining the Seq through `sink_seq_body` before falling through
to the reified-elements path. Pinned by `t/oo/method/method-definite-return-sinks-map.t`.

## `.=new` on an already-populated `SetHash`

`.new` is inherited from `Mu`, so `$x .= new` dispatches on whatever `$x` *currently* holds, not on
its declared type. `my SetHash $set .= new;` calls `.new` on the type object and worked; a later
`$set .= new;`, once `$set` holds a populated concrete `SetHash` value, calls `.new` on that value
instead — and mutsu's generic `.new` fallback (`dispatch_new`'s final "basic types" match) had a
case for the `SetHash` **type object** but none for a concrete `Set`/`Bag`/`Mix` **value**, so it
fell through to `X::Method::NotFound: Unknown method value dispatch (fallback disabled): new`.
`Game::Entities`'s `t/entities.t` reuses one `my SetHash $set .= new;` across several assertions,
so its second use died.

Fixed by giving `ValueView::Set`/`Bag`/`Mix` their own arms there, routing back through
`try_native_quanthash_construct_for_package` using the value's own `declared_type` (falling back to
the mutable/immutable base name when a value carries none). Pinned by
`t/collections/set-bag-mix/quanthash-reassign-new.t`.

## What's still open

`t/entities.t`'s remaining failures trace to a third, larger gap: mutsu's generic list coercion
(`.map`, `for`, ...) doesn't consult a user-defined `.iterator` method override on an arbitrary
class — filed as #8547. And `t/sorting.t`, a different file in the same distribution, hangs on a
`multi method sort($c, $comparator --> Nil)` candidate; confirmed to predate this fix (reproduces
identically on the pre-fix commit) and filed separately as #8552.
