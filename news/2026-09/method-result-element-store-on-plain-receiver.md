# `X.method[i] = v` on a plain receiver stores into what the method returned

`my $p = [1,2]; $p.Array[1] = 8` died in mutsu with "cannot assign through
.Array on non-instance". In raku it is `($p.Array)[1] = 8`: the store lands in
the fresh Array that `.Array` returned, that copy is thrown away, and `$p` stays
`[1 2]` (#9208).

The element store on a method result (`builtin_index_assign_method_lvalue`)
always finished by calling the method again as a setter. That is right for an
attribute accessor on an instance, and every other receiver was refused. #9197
had already special-cased `.self`. The rule is now general: on a receiver that
has no accessor to call back (not an instance, role mixin, Pair or package),
the store goes into the returned container in place when that container is a
real Array or a plain Hash. So `.self`, `.list` and `%h.Hash`, which return the
receiver's own container, update the receiver. `.Array` and `.clone`, which
return copies, do not. An immutable `List` return (`$p.List[0] = 9`) is still
refused.

Making that general exposed a second bug. On a plain real Array, `.Array`
returned the invocant itself as an O(1) shortcut. So `@a.Array =:= @a` was
`True` (raku says `False`), and `my $b = @a.Array; $b.push(3)` grew `@a`.
`.Array` now always builds a fresh plain Array. As in raku, the copy has no
element type and no `is default`. Pinned by
`t/collections/array/array-coercer-result-element-store.t`.
