---
title: Preserve argumented regex subrule aliases in RakuAST
---

Argumented subrule aliases now retain Rakudo's nested `Named::Args` assertion
tree in `.AST`, including dot suppression, qualified names, and empty calls.
Constructed and RakuAST-lowered grammars continue through the existing
package-aware regex matcher with both alias and original captures preserved.
