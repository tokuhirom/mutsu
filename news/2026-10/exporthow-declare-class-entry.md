# Class declarations in `EXPORTHOW::DECLARE` register declarators

The module scan now recognizes class declarations stored under
`EXPORTHOW::DECLARE::<keyword>`, including classes nested directly in `EXPORTHOW` with a
`DECLARE::` name. The keyword uses the same runtime HOW lookup as a `DECLARE` constant, so both
`package EXPORTHOW` and `my package EXPORTHOW` can install class-based declarators.
