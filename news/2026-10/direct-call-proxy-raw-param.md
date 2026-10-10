# A direct sub call keeps a Proxy argument for an `is raw` / `is rw` parameter

`typed-raw($attr.get_value($obj))` FETCHed the Proxy at the call site, so an `is raw` / `is rw`
parameter that assigns into it died with "Cannot assign to an immutable value". The direct
`CallFunc` path now leaves a Proxy unfetched when it lands on a positional container-binding
parameter of the (non-multi) user sub, so the binder installs it and the assignment fires STORE
(ADR-0040 §9). Fixes #12590.
