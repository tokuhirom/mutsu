# `try` sinks a Failure that a closure returns through its container

`try` runs its body under `use fatal`, so `try { $code(); ... }` with a closure `{ $failure }`
must throw. mutsu kept the "a block returning its last statement's container sinks nothing
inside it" exemption even under `use fatal`, so `Test::dies-ok { $obj }` on a Failure held in
a variable (Math::Angle's `dies-ok {$a4}`) passed silently. The exemption now holds only
outside `try` / `use fatal`; Math::Angle's three test files match raku.
