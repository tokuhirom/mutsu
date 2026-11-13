# A `--> Nil` routine no longer walks the List or Array it ends with

A routine with a definite return (`--> Nil`, `--> 42`) sinks the value of its
last statement, so that a trailing `.map` or `gather` still runs. mutsu's
sink also recursed into every element of a List or an Array. That cost
O(elements) on every call of a method like
`method !push-line($t --> Nil) { @!lines.push($t) }`, because `push` answers
the whole array. It also ran a `.map` that was merely held in the returned
array, where rakudo leaves it alone.

The sink now runs only a lazy list or an unconsumed Seq. A List or an Array is
sunk as a container. MCP::Server::Tool::Web's renderer pushed a line per
element of a megabyte document, and `@!lines.push` went from ~60 µs back to
~5 µs per call.
