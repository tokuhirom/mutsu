# `repl()` runs in the caller's lexical scope

The core `repl()` routine now reads lines from the current dynamic `$*IN`,
evaluates them in the running interpreter, and resumes the caller at EOF.
Consequently, code entered through `repl()` can read and mutate the caller's
lexical variables. The routine also works with piped input and in builds that do
not include the native line editor.
