Regex code assertions now ignore braces inside quoted strings when finding the
end of the assertion body. This fixes regexes and grammars that use `'{` or
`'}'` in assertion code. Closes #8209.
