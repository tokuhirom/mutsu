# A regex code block no longer reads the previous match's captures

A `{ ... }` block or `<?{ ... }>` assertion inside a regex that had not captured anything yet saw
the `$0` (and `$1`, ...) left behind by an earlier, unrelated match in the enclosing scope. The
positional captures beyond the in-progress match's own are now bound to `Nil` for the block, as in
Rakudo (#11740).
