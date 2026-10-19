# Set operator overrides respect their spelling

The ASCII and Unicode set operators now select user candidates under their
own routine names. Defining an `infix:<(|)>` candidate no longer changes
`∪`, and a Unicode candidate does not replace the ASCII operator. This
applies to all six binary set and bag operators. The compiler preserves the
source spelling in bytecode, including lexical operator resolution, while
both spellings continue to use the same built-in algebra implementation.
RakuAST round trips preserve the Unicode spelling and list associativity.
