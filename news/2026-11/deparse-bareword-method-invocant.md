# A paren-less sub call before `.method` is a `Call::Name::WithoutParentheses`

`make-seq.tail` (a declared sub called without parentheses and used as a method invocant) was
parsed as a plain call, so `.AST` showed `Call::Name` and `.DEPARSE` printed `make-seq().tail`.
The call is now marked as written without parentheses, as rakudo's tree has it, and deparses
as `(make-seq).tail`.
