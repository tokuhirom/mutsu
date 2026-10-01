# Statements no longer split silently at an early parser stop

The statement parser used to return early in a few expression shapes and
leave the rest of the line for the statement list, which took it as further
statements with no separator between them: `{ 42 }() orelse say "x"` ran
`orelse` as a bare word and `say "x"` on its own, `"a" ~~ /a/ eq "x"` left
`eq "x"` behind, and `temp our $out = ''` dropped the `temp`.

Each of those now parses as one statement:

- A bare-block call such as `{ ... }()` hands a following operator to the
  expression parser.
- A comparison after a regex smartmatch (`X ~~ /re/ eq Z`) continues with the
  match as its left operand.
- `temp my/our $x = ...` runs the declaration and then temporizes the variable
  (restoring the initializer's value, as rakudo does).
- Adjacent colonpairs after a colonpair term (`my @a = :a:!b:42c`) are adverbs
  that rakudo evaluates and drops; argument lists and `[...]` composers keep
  every pair (`f(:a :b)`, `[:a :b]`).
- `$@` is the item contextualizer over an anonymous array, and a sigil followed
  by a bare `::` (`%::{''}`) is rakudo's "Variable '%' is not declared".
- A routine declaration takes a statement modifier on its line
  (`sub a() { } given 3`), and `has method m() { ... }` is a method
  declaration.
- `$x .= new: ... andthen ...` is `($x .= new(...)) andthen ...`, and a
  ternary whose else branch is a `do { ... }` block ends at the newline after
  it (zef's `Zef::Client` install phase).
- A statement-level call whose last argument ends in a block
  (`subtest 'x' => { ... }`) ends at the newline after that block, so the next
  line's `if COND { ... } else { ... }` is its own statement instead of a
  modifier with a stray block.

With those early stops gone, the same-line check added for #9918 fires for any
term left on the line, not only an undeclared word infix: a statement that
stops in front of another term is "Two terms in a row", as in rakudo. When the
rest of the line does not parse at all, its own error is reported instead
(`for 1, 2 {` is a missing block, `1, => 2` an infix in term position) (#10257).
