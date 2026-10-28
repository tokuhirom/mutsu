# `.map`, `.head` and kin pull a class's own lazy `iterator` on demand

`Any`'s list methods on an instance whose class declares its own `iterator`
used to drain that iterator into an array before the method ran, so a
never-ending (`is-lazy`) iterator hung `Nat.new.map(...)`, `.head(3)` and
`.grep(...)`. The iterator is now wrapped in a deferred `Seq` (as
`Seq.new($iterator)`), which asks it `is-lazy` and pulls only what the method
needs. Found with `Math::Sequences` (#15288); its remaining load-time hang is
tracked separately (lazy `@`-array assignment from an iterator Seq).
