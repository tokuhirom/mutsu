# `$=pod` is an Array of real `Pod::*` blocks now, not a nested one-element list

`for $=pod -> $item { $item.contents }` died with
`No such method 'contents' for invocant of type 'List'`, and
`$=pod.WHAT` reported `(List)` where rakudo reports `(Array)`. The ticket
guessed two causes — an extra list wrapper around each entry, and `=head1`
producing a generic `Pod::Block::Named` instead of a `Pod::Heading`. Both
guesses were wrong; measuring against `raku` v2026.06 found a different, and
much narrower, root cause.

## Root cause

The Pod tree itself was already correct: `$=pod[0]` *was* a
`Pod::Block::Named`, `$=pod[0].contents[0]` *was* a `Pod::Heading`, and both
answered `.contents`. What was wrong was the *container*:

- `collect_pod_blocks` stored the document with `Value::array(...)`, which
  builds an `ArrayKind::List`, so `$=pod.^name` was `List`.
- More importantly, `$=pod` was treated as an ordinary `$`-sigil scalar. The
  compiler's `normalize_for_iterable` wraps a scalar variable's `for` source
  in a one-element `ArrayLiteral` (that is what makes `my $x = [1,2,3]; for
  $x` iterate once), and `compile_assign` emits `ItemizeVar` for `@a = $x`.
  In rakudo `$=pod` is *bound* to the collected document rather than stored in
  a Scalar container, so `for $=pod` iterates the blocks and `my @a = $=pod`
  copies them.

So `for $=pod` handed the whole document to the loop body as a single item —
and that item, being the container, had no `.contents`.

## Fix

`$=pod` is built with `Value::real_array` now, and the compiler treats a
`$=`-prefixed document variable as a non-container scalar: a new
`Compiler::scalar_var_is_item_container` predicate (used by
`normalize_for_iterable`) and an `ItemizeVar` guard in `compile_assign` both
exempt names starting with `=`, alongside the existing `constant` and
`:=`-bound-to-non-itemized exemptions.

The same campaign fixed several related object-model gaps found while
establishing the real model with `raku`:

- `Pod::Heading.level` is an `Int` (`=head2` → `level => 2`), not a `Str`.
  `Pod::To::Text`'s `given $pod.level { when 1 {...} }` depends on it.
- `=begin comment ... =end comment` built a bare `Pod::Block`; it builds a
  `Pod::Block::Comment` now, so the vendored `Pod::To::Text`'s
  `when Pod::Block::Comment { '' }` matches instead of falling through to
  `default { $pod.Str }` and printing a literal `Pod::Block()` into the
  rendered document.
- `Pod::Defn` and `Pod::Block::Declarator` are registered classes deriving
  from `Pod::Block`, so `$defn ~~ Pod::Block` is `True`.
- `=for code` and the abbreviated `=code` produced a `Pod::Block::Named` whose
  body had been re-wrapped into a paragraph — folding the code's newlines into
  spaces and destroying it. Both build a `Pod::Block::Code` now.
- An *explicitly* marked code block (`=begin code` / `=for code` / `=code`)
  keeps rakudo's `contents` shape: one element per source line followed by a
  literal `"\n"`. (An *implicit*, indented code block stays one joined string,
  which is what roast `S26-documentation/04-code.t` pins.)

## Result

The vendored rakudo `Pod::To::Text` (`modules/Rakudo-Core/`) now renders a
document exercising headings, formatting codes, items, code blocks, comments,
definitions and tables **byte-for-byte identically to `raku`**; before, it
emitted a stray `Pod::Block()` line and dropped the code block's trailing
newline. All 27 `roast/S26-documentation/` files still pass. Pinned by
`t/pod-object-model.t`, which passes under both `raku` and `mutsu`.
