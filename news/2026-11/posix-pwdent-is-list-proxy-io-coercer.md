# `is List` iterates a lone Iterable, typed arrays FETCH Proxy elements, `IO($x)` exists

Three gaps found with the `POSIX::PWDENT` distribution (both test files now print what `raku`
prints).

- `my @l is List = $iterable` iterates a lone `Iterable` initializer (a Range, an instance of a
  class that does `Iterable`, also one held in a `Scalar` such as `$*PWDENT`), in statement and in
  expression position; the expression form never stashed the raw initializer for the container trait.
- A `Proxy` among the initializer of a typed array declaration (`my Str @t = ($proxy,)`) or of a
  typed `@` attribute is FETCHed before the element type check, as for an untyped array. NativeCall
  `CArray` reads yield Proxies, so `has Str @.members = do while ... { $!_mem[$c++] }` died.
- `IO($x)` is the coercer form of `$x.IO`.
