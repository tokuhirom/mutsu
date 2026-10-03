# A `|c` capture keeps the caller's containers

Raku binds a Capture parameter raw, so `sub f(|c) { g(|c) }` forwards the very
containers `f` was called with. mutsu's `|c` binder stripped every argument to
its value, so `sub g($x is rw) { $x++ }; f($a)` died with "expects a writable
container" and `method new(|c) { self.bless!add-items: |c }` could not reach a
private method's `is rw` parameter (Intl::CLDR's `MonthWidth`).

The binder now stores each positional read from a plain scalar caller variable
as the same shared cell an `is rw` parameter binds, and records a per-element
writeback (the `*@v is raw` slurpy's encoding, now also readable from a
Capture), so a write through a forwarded slip or through `c[0] = ...` reaches
the caller's variable. Literals, type objects and sigilless value bindings are
still passed as values (#11295).
