# A parenthesized assignment on an assignment's right stays a group

The right side of an assignment took a shortcut for a parenthesized
assignment (`($b = 2)`) and dropped the parentheses. Two things followed:

- RakuAST lost them, so `10 R+= ($a ||= 42)` deparsed as `10 R+= $a ||= 42`.
  Rakudo keeps a `Circumfix::Parentheses` there.
- They no longer ended the inner assignment's list. In
  `my @a = (@b = 1), 2`, `@b` absorbed the `, 2` and became `[1, 2]`.
  Rakudo leaves `@b` as `[1]` and makes `@a` `[[1], 2]`.

The shortcut now keeps the group as an `Expr::Grouped`, the same way the
general parenthesized-expression path does. Found through the `.DEPARSE`
corpus (`S03-metaops/misc.t`, #14880).
