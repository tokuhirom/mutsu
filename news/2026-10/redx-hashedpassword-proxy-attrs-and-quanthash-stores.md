# RedX::HashedPassword: Proxy-backed attributes, quant-hash element stores, captured infix rollback

All three baseline files of `RedX::HashedPassword` (and with them Red's model dirty tracking)
now pass by direct run. Six interpreter gaps stood between `t/020-basic.t` and the plan:

- A rejected candidate of an exported operator family (`Red::Operators`' `infix:<eq>`) left its
  partly bound parameters in the env, where a stored closure wrote them back as the caller's own
  `$a`/`$b` (#12512). `try_user_infix` now rolls the trial bind back.
- `$obj.attr = v` where the attribute holds a `Proxy` (bound by `Attribute.set_value`, as Red does
  for every column) now fires the Proxy's STORE instead of replacing the slot, and a STORE body
  that writes its own instance is no longer rolled back by a stale snapshot.
- An element store on a `Set`/`Bag`/`Mix` reached through an accessor or an expression
  (`$o.seen<k>++`, `$attr.get_value($o).{$k}++`) now goes through `ASSIGN-KEY` and mutates the
  shared node, instead of being dropped.
- `Set()` / `.Set` / `.Bag` / `.Mix` on a scalar object with a role mixed in keep that object as
  the element; only an aggregate's mixin folds into its elements.
- A typed `is raw`/`is rw` parameter handed a bare `Proxy` binds the Proxy, so assigning to it
  fires STORE (`deflate(Str $password is raw)`).

Residue filed as #12590, #12591 and #12592.
