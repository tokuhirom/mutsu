# CSS::TagSet: six interpreter fixes found by its test suite

Working through the CSS::TagSet distribution (CSS::Properties, CSS::Module,
CSS::Grammar) surfaced a chain of independent bugs, each now fixed with a
focused regression test:

- A role group declared in a module (`role R[...]` plus a same-named `role R`)
  mixed in the wrong member: role ids minted in a module's content session have
  the high bit set, so storing them as an `Int` made them negative and the
  mixin lookup (`id > 0`) discarded them.
- A subset's `where` block that names an imported type (`$_ ~~ Resolution`) now
  closes over its declaring scope instead of resolving in the caller's.
- `&!attr()` calls the invocant's private code attribute, not a same-named
  private attribute of a caller further up the stack.
- A pointy `with X -> \v { f() }` no longer leaves the element source pending,
  which made a `with` inside `f` write its topic back into `X`.
- The else block of `with %h<k> { } else { $_ = v }` stores into the element.
- `$obj.name = v` goes through an `is rw` `FALLBACK` when the class declares no
  method or accessor `name`.

CSS::TagSet's `t/tag-set-xhtml.t` now passes 9 of 12 assertions (it died at the
first). The rest need two larger gaps, filed separately: a pointy
`given EXPR -> $p` must leave `$_` alone, and a `for` pointy variable must
shadow a same-named variable captured by a sibling multi candidate.
