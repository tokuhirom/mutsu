# Attribute metamethods are answered from their rows directly

An `Attribute` instance (what `.^attributes` and a trait handler hand out) has
no dispatch shape in the method table, so a call such as
`$attr.get_value($obj)` fell through the interpreter's whole method cascade
before reaching `Attribute`'s own row: about 22k of the 42k instructions a
`get_value` cost. The VM now answers such a call from the row directly when
the receiver is an `Attribute`, a user subclass of it, or an attribute that
`does` a role (`Attribute+{R}`), as long as no method the class or a mixed-in
role declares, and no user `^find_method`, could claim it.

`get_value` drops from 10.2 us to 4.3 us per call on a release build. AttrX::
Mooish calls it several times per lazy-attribute access; its per-object round
(construct, predicate, fetch, clear, refetch) went from ~4.2 ms to ~4.0 ms,
part of #17335. The larger remaining cost there is the AttrProxy methods
reached through `.VAR` (#14855).
