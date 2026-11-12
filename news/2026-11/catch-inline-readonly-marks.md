# A CATCH handler no longer sees the thrower's read-only parameters

A `CATCH` handler runs at the throw site, above the frames between its `try`
and the `die`, and it ran with their parameters' read-only marks in effect:
after `$obj.unsupported($reason)` died, the handler's own
`$reason = .message` failed with "Cannot assign to a readonly variable or a
value", because the method's `Str $reason` parameter was marked read-only by
name. A handler run inline now sees the read-only marks of the frame that
installed it. Red's `red-compile` CHECK pass caught its unsupported-callback
exceptions this way.
