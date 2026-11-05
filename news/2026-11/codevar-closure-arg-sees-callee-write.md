# A closure argument sees a callee's write through a code-variable call

`my &wrap = -> |c { root |c }; wrap $e, { say $e }` printed an empty `$e` while
`root` assigned it, where Raku prints the written value. A direct sub call already boxed the
variable into a shared cell before creating the closure argument; a call through a code variable
or callable value did not know the callee, so the closure captured a snapshot. A plain scalar
local that is followed by a closure literal in such a call is now counted as possibly written
by the callee, so the closure shares its cell. Also fixes the immediately invoked
`(-> |c { root |c })($h, { ... })` form. The issue's two remainders (#16179) are closed.
