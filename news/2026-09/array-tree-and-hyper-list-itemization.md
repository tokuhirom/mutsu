# Remove extra itemization in Array tree and List conversions

`.tree(1)` on an Array now maps element values rather than their Scalar
containers, preserving the requested tree depth. `.List` also decontainerizes
elements when its receiver is an itemized Array. This makes a hyper `.List`
followed by `.flat` flatten nested values as Rakudo does, while explicit
itemization inside an immutable List remains intact.
