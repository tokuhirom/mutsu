# Log::Reader: dynamic-variable autopairs, associative `.Slip`, alias-free action dispatch

Log::Reader 1 (`t/01-tests.t`, 8540 assertions) failed 249 of them under mutsu
and passed only 107. It now passes all 8540 with no stray warnings. Three
general bugs were behind it:

- **`{ :%*DIRECTIVES, :@*ROWS }` parsed as a Block.** The brace classifier
  treated a leading `:$var` autopair as a hash composer only when the twigil
  was `!`. It now accepts every twigil the colon-pair parser accepts (`*`, `?`,
  `.`, `^`, `=`, `~`, `:`). So a grammar action's `make {:%*D, :@*R}` makes a
  Hash instead of an anonymous Block.
- **`.Slip` on a Set, Bag, Mix or Hash wrapped the whole container.** In
  rakudo, `Any.Slip` is `self.list.Slip`, so an associative slips its pairs.
  mutsu gave `slip(set(),)`, so an empty set difference counted as one element.
- **An alias on a non-subrule atom fired an action method.** For
  `token remark { 'Remark:' $<remark> = <:!Cc>* }`, mutsu called
  `method remark` twice. The second call was for the alias capture, with no
  `$<remark>` inside, so it warned "Use of Nil in string context". Rakudo
  dispatches actions only when a subrule reduces. Now an alias on a group, a
  character class or a quantified atom gets an empty rule name, the same as a
  positional `( )` group. The action walk still descends into that alias's
  own captures.
