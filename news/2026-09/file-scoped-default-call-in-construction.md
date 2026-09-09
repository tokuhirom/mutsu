# File-scoped subs remain visible to construction-time defaults

Attribute initializers and `BUILD` parameter defaults now resolve bare calls
in the compilation unit that declared their class. This keeps a private helper
beside a class visible when another module constructs it, including both
`has $.value = helper()` and `submethod BUILD(:$!value = helper())`.

The construction paths already knew the class or method declaration involved,
but evaluated these expressions while the caller's compilation unit was
active. That made compunit-private helpers look undeclared after module loading
moved them out of the shared `GLOBAL` routine table. The regression is pinned by
`t/attr-default-file-scoped-call.t`.
