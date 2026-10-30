# `callsame` out of a mixed-in role's method picks the most specific class multi

`callsame` / `nextsame` inside a method of a role added with `but` deferred to
the receiver class's candidates in declaration order, so a role-composed
catch-all `multi method m($x)` (or any earlier multi) beat a more specific one.
The frame is now built from the same ranked deferral expansion every other
method frame uses. Red's `ScopedDriver.map-exception` relies on it: a unique
violation inside a transaction was mapped to `UnknownError` instead of
`X::Red::Driver::Mapped::Unique` (t/98-savepoints).
