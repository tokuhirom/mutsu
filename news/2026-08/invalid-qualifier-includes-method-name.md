# Invalid qualified-method errors include the method name

`X::Method::InvalidQualifier` now reports the actual method being dispatched,
such as `split`, instead of the vague phrase “a method”.
