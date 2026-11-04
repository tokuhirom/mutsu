# `undefine` is reported as a deprecated core routine

`undefine $x` still undefines, but each use now records a core deprecation that
is printed at exit exactly as rakudo prints it:

```
Saw 1 occurrence of deprecated code.
================================================================================
undefine seen at:
  script.raku, lines 3,4,5
Will be removed with release v6.e!
Please use another way: assign a Nil; for Arrays/Hashes, assign Empty or () instead.
--------------------------------------------------------------------------------
```

The deprecation report gained a "Core" entry kind for this form (a routine named
by itself with the release that removes it). This is the fourth subtest of
`roast/MISC/misc.t` (#16412).
