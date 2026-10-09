# Native base candidates no longer use an exhaustion probe list

The grammar `parse` and `Mu` (`new`, `BUILDALL`, `POPULATE`, `clone`) deferral bridges are `Native` frame entries pushed by
the frame builder, like the storage, metamodel and grammar-rule ones before them. `NativeBase` and the probe lists are
deleted (ADR-11276 §9.45, part of #12423).
