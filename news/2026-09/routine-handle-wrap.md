Routine handles for builtin and proto-backed routines now support `.wrap` and
`.unwrap`. Wrappers installed through a handle such as `&dir` intercept normal
named calls as well as indirect calls through the handle.
