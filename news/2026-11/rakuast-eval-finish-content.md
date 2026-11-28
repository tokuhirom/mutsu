RakuAST EVAL now retains a parsed unit's `=finish` section, including when
heredocs contain lines that look like `=finish`. String EVAL also exposes its
finish section. Both forms restore the caller's `$=finish` after evaluation,
including when the evaluated unit loads a module.
