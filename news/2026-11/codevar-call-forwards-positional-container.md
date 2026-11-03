# A `|c` capture sees a variable passed through a code variable

`my &wrap = -> |c { root |c }; wrap $e` handed `root` the value of `$e`: an
undefined `$e` reads as a type object, and the `|c` binder took a type object
that carried no slot tag for a bare type name (`f(Int)`), so it kept no
container and an `is raw` / `is rw` parameter died with "Cannot assign to an
immutable value". The binder now also asks the calling frame whether the
argument's name is one of its declared locals: a local is a variable whatever
it holds. A block argument that reads the variable while the call is still
running still sees the old value; that is recorded on #16179.
