Method declarations require separators after their bodies
--------------------------------------------------------

Direct method, multi method, and submethod declarations in a class or role now
require a semicolon, newline, or other valid separator before the next
statement. Same-line declarations without a separator produce the syntax error
Rakudo reports. Lexical submethods and declarations separated by a semicolon or
newline keep working.
