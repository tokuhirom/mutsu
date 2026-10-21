# A failed grammar parse's diagnostic probe no longer records a binding error

When `Grammar.parse` fails, mutsu re-walks the pattern with its code blocks
disabled to find how far the parse got. A subrule argument that names a rule's
`:my` variable (YAMLish's `<value=map($new-indent)>`) reads `Nil` in that walk,
because the assignment lives in a skipped code block, so binding it to a
`Str $indent` parameter failed and left a "got Nil" error in the thread-local
regex error slot. That is the `Nil` behind #13612: Rakudo never makes a second
walk. The probe now keeps the slot as the real parse left it.
