---
title: Preserve method-call expressions in regex subrule arguments
---

Ordinary method-call expressions in regex subrule arguments now retain their
RakuAST call tree and lower back through the existing regex parser. Match-time
evaluation continues to observe the current lexical value.
