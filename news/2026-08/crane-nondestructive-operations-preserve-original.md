# Crane non-destructive operations preserve the original container

Assignments through an argument-taking package `is rw` method, such as
`Crane::At.at($root, @path){$step} = $value`, used the generic index-assignment
path. That path lost the identity of the container returned by the accessor and
could retain a same-named sigilless alias from the caller. As a result, Crane's
non-destructive operations mutated the caller's container and returned their
unmodified deep copy.

The compiler now lowers these assignments through the lvalue-method helper,
including the method arguments and the local that owns the accessor's first
argument. Package accessors mutate the returned hash or array directly, then
detach and write the updated root back to its owning local before the accessor's
temporary `return-rw` containers are unwound. This crosses the accessor
boundary without redirecting the update through a same-named caller alias.

`Crane.add` now leaves the input untouched and returns the updated deep copy,
matching Raku. The hash and array forms are pinned by
`t/package-rw-method-index-assign.t`.
