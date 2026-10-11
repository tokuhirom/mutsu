# Postfix `.&{ ... }` parses

`.&{ block }` (an inline block as the callee of the `.&` postfix) used to be a syntax error.
The postfix parser now accepts a block after `.&` and calls it with the invocant, like `.&(...)`.
