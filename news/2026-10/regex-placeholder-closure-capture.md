# A regex returned from a block keeps its placeholder arguments

`{ /$^a$^b/ }` builds a regex from the block's two placeholder arguments. Matched inside the
block it worked, but once the regex was returned (as `(/^/, |@parts, /$/).reduce({ /$^a$^b/ })`
does, the idiom Path::Finder's `join-regexps` uses for its glob support) it never matched: the
regex literal's closure scan recognised `$name` but not the `^` twigil, so `$^a` was not captured
and resolved to nothing at match time. The scan now keys a placeholder as `^a`, the name the
block's implicit signature binds, and Path::Finder's `t/glob.t` passes (#13149).
