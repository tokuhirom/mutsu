# A subset of a coercion type ranks like the coercion parameter in multi dispatch

`subset CC of Str() where ...` resolved to the base `Str()`, which the type
hierarchy walk found nowhere, so a candidate typed `CC` ranked as unrelated
and lost to a bare `$` candidate. The base is now ranked as the coercion
parameter itself (narrow as `Str` for a Str argument). Regression of
Business::CreditCard (#15426).
