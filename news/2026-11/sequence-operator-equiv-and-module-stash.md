# Sequence operators declared with `is equiv(&infix:<...>)` are called; a module file's stash stays clean

Found with Sequence::Generator (#16875):

- A user-declared `infix:<...>`/`<^...>`/`<...^>`/`<^...^>` carrying `is equiv(&infix:<...>)` sits at the
  sequence precedence level, where the built-in token always won, so `$a ^... Inf` inside the module
  ran the built-in operator (`27.0, 9.0` instead of `27, 9`). The declared operator is now called, and
  takes a bare `*` endpoint un-curried like the built-in.
- `class X {...}` followed by file-scope `sub ... is export` in `X.rakumod` listed those routines in
  `X::.keys`, and a `my role` inside a class listed a mangled name; neither is a stash member in rakudo.
- `Seq.new($unbounded-iterator).lazy` and `so Seq.new($it)` no longer drain the iterator: `.lazy` hands
  the iterator to a lazy Seq, and `Bool` pulls only the first element.
