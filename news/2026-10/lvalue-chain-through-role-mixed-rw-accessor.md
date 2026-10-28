# Element stores through an rw accessor's role-mixed object

`$doc.data<a><b>[1] = v` where `data` is an `is rw` accessor returning a
`Proxy`, and the `Proxy` FETCHes an instance with a role mixed in
(`$obj but Role`) that serves `AT-KEY`/`AT-POS` through the role (Red's
`has Json $.data is rw is column`), stored nothing: the chain's compiler temp
held the unfetched `Proxy`, `object_subscript_accessor` only looked at plain
instances, the two-level and last-level stores only handled plain instances,
and the one-level store only tried `ASSIGN-KEY`/`ASSIGN-POS`.

The chain now steps through the fetched, role-mixed object, and every level
shares one helper (`assign_through_object_location`) that stores through the
object's `ASSIGN-*` or the location its rw `AT-*` returns. This fixes Red's
"Dynamic array update" and its siblings (Red t/87).
