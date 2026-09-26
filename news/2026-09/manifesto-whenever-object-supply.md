# `whenever` coerces object sources through `Supply`

`whenever` now applies the normal `.Supply` coercion to object sources before registering a react subscription. This lets user-defined stream-like objects work when passed directly to `whenever`, as in Manifesto 0.0.7's `whenever $manifesto` synopsis, while preserving the native handling for `Supplier`, `Proc::Async`, listeners, promises and channels.
