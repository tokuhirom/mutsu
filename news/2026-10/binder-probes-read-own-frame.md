# The argument binder stops reading its caller's `@_` and `self`

Binding a call's arguments asked for two names in the whole env chain.
A call with no arguments skipped binding `@_` when an empty `@_` was
visible, and the visible one was usually the caller's. Every call also
fetched `self`, only to bind an invocant's type capture (`::T $self:`), so a
sub called from a method found the method's invocant. The `@_` probe now
reads the running frame only, and the invocant probe runs only for a
signature that captures the invocant's type.

Neither read changed an answer, because the caller's `@_` is copy-on-write.
They were the largest group of lookups that still resolved in a caller's
frame, and that dependency on the caller chain is what ADR-12529 removes.
A census over `t/` counted 234,362 such lookups before and 122,438 after.
