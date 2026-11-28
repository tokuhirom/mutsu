# Bare blocks share their creation topic

A bare block called without an argument now reads and assigns the live `$_`
binding of its creation scope, including when called immediately. Assignments
made after the block is created are visible inside it, and assignments inside
it reach the outer scope. Later loop and `given` topic bindings remain separate
from the captured binding.
