# Enums exported from a parameterized role are importable

A `my enum … is export` declared in a parameterized role body was not
declared until the role was first composed. Algorithm::Treap has this
shape: `unit role Algorithm::Treap[::KeyT]; my enum TOrder is export <DESC ASC>;`.
The importer saw `TOrder` as a bare `Str`. The enum created at composition
time was a different type, so it rejected the caller's `TOrder::ASC` with
"Type check failed in binding to parameter '$!order-by'".

An enum's variants are compile-time constants and cannot depend on a type
parameter. The only exception is an explicit base type (`my T enum …`). So
the role-body pass that eagerly declares lexical types now declares enums in
parameterized roles too. Other kinds of declaration in a parameterized role
still wait for composition. With this fix, Algorithm::Treap 0.10.3 passes
6/6 baseline files, up from 5/6.

Two related gaps remain: an exported `my class` or `my constant` in a role
body is still not importable. That is tracked in #9981.
