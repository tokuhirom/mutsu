# `.wrap` on a built-in method binds an `is rw` invocant to the caller's variable

A wrapper declared with an `is rw` invocant (`sub timezone(DateTime:D $self is rw)`) now
receives the caller's container when it wraps a method of a built-in class, so assigning to
the invocant changes the caller's variable and `callsame` re-dispatches on the updated
value, as in Rakudo. The method-site and scalar early lanes also no longer bypass a wrapped
built-in method.
