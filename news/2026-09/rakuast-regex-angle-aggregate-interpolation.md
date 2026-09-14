---
title: Preserve aggregate angle regex interpolation in RakuAST
---

`<@name>` and `<%name>` now retain Rakudo's
`RakuAST::Regex::Assertion::InterpolatedVar` shape, including the aggregate
lexical sigil and sequential-branch context. Array-valued assertions continue
to resolve their elements at match time, and constructed RakuAST nodes lower
through the normal Parser -> Compiler -> VM path.
