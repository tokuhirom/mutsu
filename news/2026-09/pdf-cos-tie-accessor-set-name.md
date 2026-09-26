# PDF::COS::Tie's entry accessors: end-of-line braces, set_name, ^add_method with a sub

Nine PDF::Font::Loader modules failed to load with `Unknown function: accessor`
(#9479). The error came from PDF::COS::Tie's attribute HOW:

```raku
my &accessor = sub (\obj) is rw { obj.rw-accessor( self, :$key ); }
&accessor.set_name( $key );
try $package.^add_method($key, &accessor);
```

Three separate gaps were in the way.

- **A block's `}` at the end of a line ends the statement.** mutsu honored that
  rule for one block form and one precedence layer, the chaining comparisons
  (`before`, `eq`, ...). Everywhere else it read the next line as an infix, so
  the two lines above parsed as `sub {...} & accessor.set_name(...)`, an
  all-junction with a call to a function `accessor`. Every block-body parser
  now records the end-of-line `}` (`block_inner` and the three
  `parse_block_body*` variants). Every shared infix-operator matcher now
  declines at that position: the junction, additive, multiplicative, concat,
  replication, logical, feed, meta, bracket, infix-term, custom-word and
  flip-flop operators. So a `&code`, `-1`, `~"x"`, `%hash` or `|slip` line after
  a `sub`/`method`/pointy/`do`/value block is a new statement, as in Rakudo. A
  subscript's or a quote's `}` is not a block and still continues.
- **`Code.set_name`** fell through to method composition and answered a
  `<composed-method:set_name>` Sub, leaving the name unchanged. It now renames
  the code object in place, and every alias sees the new name.
- **A `sub` passed to `^add_method`** now receives the invocant as its first
  positional, as a bare or pointy block already did. Only method code keeps the
  implicit invocant. A declared routine (`&named-sub`) cannot take this path
  yet; that is #9549.

`use PDF::Font::Loader::Dict` now loads. `PDF::Content::Ops` gets past the same
error and stops at its next blocker, a parenthesized multi-attribute
declaration with defaults.

Tests: `t/control/block-brace-ends-statement.t` and
`t/oo/method/code-set-name-and-add-method-sub.t`.
