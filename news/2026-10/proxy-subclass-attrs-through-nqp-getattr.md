# AttrX::Mooish's AttrProxy: `nqp::getattr` on a Proxy, and `my Mu` binds

Two ways a Proxy was FETCHed where Rakudo leaves it alone:

* `nqp::getattr($proxy, AttrProxy, '$!is-set')` FETCHed the Proxy and then asked
  the fetched value for the attribute, so it read `Nil`; and a Proxy subclass's
  own attributes were not found at all (`nqp::bindattr` ignored them too). The
  attribute ops now take the Proxy itself and read and write its attribute cell.
* `my Mu $x := Proxy.new(...)` type-checked by FETCHing the Proxy, running a lazy
  attribute's builder while its AttrProxy was only being installed. `Mu` accepts
  anything, so the bind no longer looks at the value (a narrower constraint still
  checks the Proxy's current value, as Rakudo does).

AttrX::Mooish 1.0.10: `t/010-base`, `011-arrays`, `014-private`, `016-filtering`,
`020-role`, `030-inheritance` and `050-multi-filter` pass in full (#14266).
