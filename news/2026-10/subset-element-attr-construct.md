# Subset element types of `@`/`%` attributes are checked at construction

`has ne @.r` (with `subset ne of Str where *.chars > 0`) now rejects a failing
element in `.new`, like Rakudo. The constructor's element-type gate only accepted
uppercase class names; a user `subset` is now accepted too. Closes #12479.
