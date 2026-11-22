# An EVAL's compile error renders under the EVAL unit

`EVAL "1 +"` left uncaught printed `===SORRY!=== Error while compiling -e`,
followed by a snippet of the program that called EVAL. rakudo compiles the
EVAL unit under its name joined onto `$*CWD` (`/home/me/EVAL_0`, while the
unit's `$?FILE` stays `EVAL_0`). Its compile error names that file and shows
the EVAL'd source:

```
===SORRY!=== Error while compiling /home/me/EVAL_0
Missing required term after infix
at /home/me/EVAL_0:1
```

The EVAL call now gives its compile errors the unit's absolute name, which the
exception's `.filename` reports too, and the EVAL'd source for the snippet.
