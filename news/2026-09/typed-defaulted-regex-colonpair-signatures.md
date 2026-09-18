---
title: Typed defaulted regex colonpair signatures retain their defaults
category: fix
---

RakuAST regex arguments now preserve a simple typed scalar parameter together
with its default expression in an explicit pointy block signature. Constructed
callables enforce the type and supply the declared default when invoked without
that positional argument.
