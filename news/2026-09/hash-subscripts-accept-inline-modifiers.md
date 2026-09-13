# Hash subscripts accept inline statement modifiers

Hash and array subscripts now accept inline statement modifiers in their index
expressions, matching Raku's semilist parsing. This lets expressions such as
`%hash{S/b/c/ with $key}` use the modified expression as the key.
