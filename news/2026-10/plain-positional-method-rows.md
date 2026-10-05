---
title: "Move plain positional conversions into method rows"
---

`List.list`, `List.List` and `List.Array` now use shared built-in method-table
handlers for plain List and Array values. Shaped and lazy arrays retain their
specialized conversion paths.
