`RegexTree` now preserves explicit static `before`/`after` lookaround
assertions through `.AST` and constructed-tree `EVAL`, including positive and
negative forms. They lower through the existing regex matcher without
consuming the asserted text.
