# RakuAST: a continuation-line method call keeps its parentheses

A method call at the start of a continuation line after a parenthesized term
(`(1 ~ 2)\n    .chars`) is a dotty infix in Rakudo, whose left side keeps its
`Circumfix::Parentheses`; Rakudo drops them only around a postfix operand.
mutsu dropped them in both places, so the dotty infix's left side and its
parts had no `.origin` (#15780). The view now renders a grouped left side as
the parentheses, and the conversion moves to `dotty_infix_view.rs`.
