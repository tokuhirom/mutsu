# Autovivifying through `is rw` locations, `++` on an undefined base

`Math::Symbolic` keeps its polynomial terms in an object hash keyed by hashes. Its
`MultiHash.elem(...)` is an `is rw` method returning `%!hash{ $%key }`, and the
caller writes `$vars.elem($v => 1)[0]++`. Three gaps broke this:

- **Element store or `++` through a location.** An `is rw` routine that returns
  a missing element hands back a location (`sub f() is rw { %h<k> }`). A store
  through it (`f()[0] = 7`) was dropped, and so was a `++`. Both now vivify the
  entry into the container the subscript asks for: `[ ]` makes an Array,
  `< >`/`{ }` a Hash. The same holds for a variable bound to a missing element
  (`my $t := %h<a>; $t[0]++`), which is then bound to the new container, as in
  rakudo.
- **`++`/`--` on an undefined base.** `++`/`--` on an element of an undefined,
  untyped scalar (`my $x; $x<a>++`, `$z[1]++`) did nothing. It now
  autovivifies, like an element assignment. The four `*IncrementIndex` /
  `*DecrementIndex` opcodes now carry the subscript's positional flag, which
  decides whether that is a Hash or an Array.
- **Object-hash keys through an accessor.** Assigning pairs to an object-hash
  attribute through its rw accessor stringified the keys. This covers both
  `$o.hash = $o.hash.grep(...)` and `$o.hash .= grep: ...`. A Hash key read back
  as `"x\t1"`; the key objects are now kept.

Math::Symbolic's test file now gets through `.condense`. The rest of it needs
the longest-token choice between `<equation>` and `<expression>` in its grammar.
