# A grammar's enum member names no longer leak into other files' code

`PDF::Grammar` declares `enum AST-Types <array body ...>` inside its grammar body. Code that
was not lexically in that file still resolved a bare `array` to the enum member: a closure the
main script handed to a module routine (the running-package anchor was the *calling* routine's
package), an action class run while the grammar was current, and an action class in a file of
its own under the `PDF::Grammar::` namespace (the `::` ancestor walk crossed the file boundary).
`array[uint64].new(...)` in `PDF::Grammar::COS::Actions` then died with "Unable to call
postcircumfix:<[ ]> with a type object". A called closure now anchors on its own compunit, the
current package only anchors code that has no package of its own, and a top-level enum key
reached up the package chain must belong to the running compunit. `t/pdf-objects.t` and
`t/lite.t` of PDF::Grammar now pass.
