---
title: Preserve phaser-expression initializers in `:=` bindings
---

`:=` bindings whose right-hand side was a `BEGIN` or `INIT` expression could
lose their initializer when phaser reordering split the declaration. The
initializer is now retained, so the phaser runs and its value is bound.
