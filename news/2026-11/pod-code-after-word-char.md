# A Pod formatting code right after a word character is read

mutsu's RakuDoc reader declined a formatting code that directly followed a
letter or digit. `RakuAST::Doc::Paragraph.from-string("aB<b> c\n")` therefore
returned the plain string, and a source Pod block's `.AST` kept such text
unread. rakudo reads the code there: `a`, then `B<b>`, then ` c` (#19068).
The reader no longer looks at the character before a code letter.
