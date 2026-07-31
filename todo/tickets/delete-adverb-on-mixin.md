# `:delete` on a mixin does nothing and returns Nil

`:exists` on a mixin was fixed by routing it through EXISTS-KEY/EXISTS-POS
([news](../../news/2026-07/exists-adverb-dispatches-through-a-mixin.md)).
`:delete` has the same gap and is not fixed:

```raku
role R { method greet { 'hi' } }
my %h = a => 1;
my $m = %h but R;
say $m<a>:delete;   # raku: 1 (and the key is gone).  mutsu: Nil (key stays)
say $m.DELETE-KEY('a');
#   raku:  1
#   mutsu: No such method 'DELETE-KEY' for invocant of type 'Hash'
```

`exec_delete_index_named_op_inner` (`src/vm/vm_var_delete_ops.rs`) dispatches the
subscript protocol only for `ValueView::Instance`, and only when
`has_user_method(class_name, "DELETE-KEY"/"DELETE-POS")` says the class declares
one. A mixin is neither, so the call falls through to the plain container paths
below, which do not recognise a `Mixin` target either.

Copying the `:exists` fix is not enough, because the second half of that fix does
not exist for delete: where a role supplies no `EXISTS-KEY`, method dispatch
reaches the inner Hash's/Array's own `EXISTS-KEY`/`EXISTS-POS`. There is no
builtin `DELETE-KEY` on `Hash` or `DELETE-POS` on `Array` to reach — mutsu
implements `%h<k>:delete` directly in the opcode instead. So this needs either

* builtin `DELETE-KEY`/`DELETE-POS` methods on `Hash`/`Array` (which is what
  Rakudo has, and would also make `%h.DELETE-KEY('a')` work — it currently dies),
  after which the mixin path is the same three lines as the `:exists` fix; or
* unwrapping the mixin to its inner container in the delete opcode, which does
  not honour a role that *does* define `DELETE-KEY`.

The first is the real fix. Note `$m<a>:delete` must also mutate the container the
mixin wraps, not a copy.
