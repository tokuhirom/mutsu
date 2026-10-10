# `new` binds a parent's public attribute despite a child's same-named private one

`is_attribute_buildable` stopped at the first class declaring the bare name, so
a child's `has @!v` hid a parent's `has $.v` and `QV.new(:v(9))` dropped the
argument. It now walks the MRO and treats the name as buildable when any layer
declares it publicly (follow-up to #12528).
