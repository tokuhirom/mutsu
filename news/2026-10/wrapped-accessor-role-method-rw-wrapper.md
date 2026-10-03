# An accessor wrapped by an `is rw` role method stays assignable

Staticish turns a class into a singleton. Its HOW role wraps every method with
`self.^find_method('_rw_wrapper')`, a role method declared `($self: |c) is rw`
that redirects a type-object call to the single instance. `Bar.foo = 'test'`
then died with "Cannot modify an immutable Package (Str)".

A wrapped accessor hands its container back only when every wrapper in its
chain declares a container return. That check looked only at plain `Sub`
wrappers. A `Method` object returned by `.^find_method` carries its callable
inside, so it never counted as `is rw` and the container request was dropped.
The check now reads through to that callable.

As a result, all four of Staticish's test files pass under mutsu.
