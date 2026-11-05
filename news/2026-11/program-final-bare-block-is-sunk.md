# A bare block ending the program is sunk like any other final statement

Raku sinks a program's last statement (#9766), but mutsu only did so for a final expression or call:
a final bare `{ ... }` was compiled in value position, so a regex ending it was never matched
(`{ $_ = 'x'; /{ say "ran" } x/; }` printed nothing; rakudo prints `ran`). The final bare block of a
program is now compiled as an ordinary statement. `EVAL`, REPL lines and routine bodies keep their
tail value. Closes #16679.
