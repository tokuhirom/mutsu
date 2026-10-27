# `with` / `given` pointy blocks bind a hash pattern, so `is rw` leaves write through

`with %h<a> -> % (:$counter! is rw) { $counter++ }` copied the topic into the
anonymous `%` pattern variable, so the `is rw` named leaf aliased the copy and the
increment never reached `%h<a>`. The aggregate-sigil unpack target is now bound,
and a named `is rw` / `is raw` leaf binds the element's container as the
positional leaves already did. Red's Mock driver counts its expected queries
this way (`t/02-sqlite`, `t/05-better-map`, `t/12-roles` now pass).
