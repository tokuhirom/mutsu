# A token's slurpy parameter is visible to its code blocks

A rule's parameter values are carried into the rule body's `{ ... }` blocks
and `<?{ ... }>` assertions by baking them into the block text, because those
blocks run in the matcher's environment, not the rule's. That baking only
recognized `$name` reads and skipped slurpy parameters entirely, so

```raku
grammar G { token TOP { <a(1, 2, 3)> } ; token a(*@x) { { say @x.raku } . } }
```

printed `[]` instead of `[1, 2, 3]`, and a `*%h` came out empty too (#18670).
`@name` and `%name` reads of a bound array or hash parameter are now baked
like `$name`, slurpy ones included, and `"@x[]"` inside a string in a code
block interpolates the elements.
