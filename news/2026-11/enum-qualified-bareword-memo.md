# `Order::Less` read in a loop no longer re-resolves the name every time

A bareword such as `Int` has kept a per-site memo for one registry
generation (ADR-0121 D3) for some time. A package-qualified enum key such as
`Order::Less`, `Bool::True` or `Color::Green` was not covered: it ran the
whole bareword resolution chain on every execution, at about 1 µs each in a
release build.

The reason was the memo's `env` check. It refused any site whose name `env`
binds, and an enum declaration binds `Type::Key` to its own value, so every
enum key failed the check. The check now also accepts an `env` binding that
is the remembered value itself, comparing type and key. The memo can
therefore keep a qualified enum key's value as well as a type object.

Array::Sorted::Util compares each `cmp` result against `Order::Less` and
`Order::More` inside its binary search, and this is that loop's cost (#15786).
