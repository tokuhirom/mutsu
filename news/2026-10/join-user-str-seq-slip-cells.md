# `join` calls a user `.Str` on the objects of a Seq, a Slip or array cells

`join "|", @objs.sort(*.n)` rendered each object as `P<19>` instead of running its `Str`
method. The routine form pre-renders user stringifiers only for the elements it can see,
and it looked into arrays but not into a `Seq` or `Slip`; the gate deciding whether the
pure path is enough (`join_needs_interpreter`) also stopped at a container cell, so
`@objs.reverse` (which hands back the array's own cells) took the pure path as well. Both
now look through a Seq, a Slip and one cell. This is the last difference in App::RaCoCo's
`t/12-report-data.rakutest`.
