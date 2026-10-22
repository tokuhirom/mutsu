# An EVAL's `my \lc` / `my constant uc` no longer outlives the EVAL

The `__mutsu_constant_var::` marker a `my \name` or `my constant name` declaration leaves in the
environment was not part of the EVAL's lexical cleanup, so the next EVAL parsed `lc` as a declared
term (and evaluated it to the string `lc`) instead of calling the builtin. The marker is now
snapshotted with the declaration's other keys and restored when the EVAL returns, so a caller's
own constant of the same name comes back too (#14582).
