# `Seq.new($iterator).skip(*-3)` no longer dies with X::Seq::Consumed

`.skip` with a Callable or Whatever count on a Seq over a not-yet-pulled user iterator died with
"The iterator of this Seq is already in use/consumed". The lazy stage for `skip` handed the
receiver's iterator to a fresh Seq and spent the receiver before finding out it declines a
non-Int count; the consume step had also skipped the receiver because `skip` has a stage. The
stage now puts the iterator back when it declines, and the dispatch consumes the body itself
when the stage declined, so the eager `skip` sees the elements.
