# `lines(:close)` closes the handle at EOF

`$fh.lines(:close)` and `lines($fh, :close)` returned the lazy lines Seq
without arming close-on-exhaust, so the handle stayed open after the last
line was read (`words(:close)` already closed). Both forms now set
`close_on_exhaust`: a read that reaches EOF closes the handle, a partial
consumer leaves it open.
