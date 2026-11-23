# `-c` on a unit that compiles runs no END phaser

`mutsu -c -e 'use SomeModule; 1'` printed the module's END phaser output
before `Syntax OK`; rakudo runs no END phaser for a compile that succeeds, not
the program's own and not one a module the compile loaded registered. A
failed `-c` compile still runs the loaded modules' END phasers after the error
report, as rakudo does (#18921). The issue's other half — a parse-time
`BEGIN plan 3` before a syntax error printing Test's "You planned 3 tests, but
ran 0" — already matched rakudo after #18924 and #18926.
