`given ($x = value)` and scalar compound assignments now expose their target
container as a writable topic. Writing `$_` inside the block updates `$x`.
Array assignments and scalar bindings to immutable values retain their
read-only topic behavior.
