# A `&code(...)` call tags its variable arguments like a sub call

`my &wrap = -> |c { root |c }; wrap $e` handed `root` the value of `$e`: the
arguments of a call through a code variable were compiled without the
`WrapVarRef` tag a plain sub call gives them, so an undefined variable read as
a bare type name and a `|c` parameter kept no container for it. The code-variable
call now compiles its arguments with the same routine as a sub call, so an
`is raw` / `is rw` parameter reached through `|c` writes the caller's
variable (#16179). A block argument that reads the variable while the call is
still running still sees the old value; that is recorded on the issue.
