# ORM::ActiveRecord: method parameter defaults and `handles *` errors

Taking ORM::ActiveRecord from five regression files to parity fixed two interpreter gaps.
A method parameter default (`:$name = default-connection()`) is now evaluated after the method's
routine frame is pushed, so a routine imported by the method's own file resolves instead of dying
with "Unknown function". And `has A $.x handles *` no longer swallows an exception thrown by the
delegate's method (which re-ran the method as a side effect): only "the delegate has no such
method" moves on to the next delegate.
