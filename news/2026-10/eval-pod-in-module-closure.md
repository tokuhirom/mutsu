# EVAL'd `$=pod` inside a module closure

`EVAL` is its own compilation unit with its own `$=pod`, but when it ran inside a closure of a
module method, `$=pod` resolved to the calling module's (empty) document and `$=pod[0]` was `Any`.
The unit-lexical lookup now skips the module's `=pod` cell while an `EVAL` is running. This lets
`Pod::TreeWalker`'s table walking (`!podify`) work; all five of its test files now pass.
