# A module routine's `our` regex variable is found when it is called from a gather body

`m:i{^$re$}` in a module sub, with `our $re` at the module's file scope, died with
`Variable '$re' is not declared` when the sub was called from inside a `gather` body: the body
runs over a snapshot of its creator's env, which lacks the loader-scoped `our` entry the sub
normally finds. The interpolation lookup now falls back to the `our` store from inside a routine
declared in a file (#16918).
