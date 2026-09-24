# `$p.self[0] = 5` stores into a plain Array or Hash

An element store through `.self` on a plain (non-instance) receiver died with
`X::Assignment::RO: cannot assign through .self on non-instance` (issue
#9197): `my $p = [1,2]; $p.self[0] = 5`, and `$r.self[1] = 7` where `$r`
aliases `@b`. `@b.self[0] = 9` and the `is Array` instance form (#9168)
already worked.

`builtin_index_assign_method_lvalue` copied the container the accessor
returned, propagated the copy to aliases, then wrote it back by calling the
accessor as a setter -- a step that refuses every non-instance receiver. Since
`.self` hands back the receiver's own container, the store now goes into that
container in place (when it is the same mutable `Array`/`Hash` the receiver
holds), with no setter write-back.

An immutable `List` is still refused, now with rakudo's message ("Cannot
modify an immutable List ((1 2))") and before the copy-and-rebind step, which
had been leaking the refused write into the variable (`my $l = (1,2);
$l.self[0] = 5` died but left `$l` as `(5 2)`).

Pinned by `t/collections/dot-self-element-store.t`.
