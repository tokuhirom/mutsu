# `chdir` inside a sub now updates the caller's `$*CWD`

`chdir` wrote the new `$*CWD` straight into the running frame's environment, so the
change vanished when the enclosing sub returned unless `chdir` was the last thing it did.
The builtin now records the write as a dynamic-variable write, the same way an assignment to
`$*CWD` does, so it survives every return. This is what P5chdir's `chdir` wrapper
(`so &CORE::chdir($s)`) needs.
