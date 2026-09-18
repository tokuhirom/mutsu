---
title: Typed regex colonpair signatures retain their parameter constraints
category: fix
---

RakuAST regex arguments now preserve a simple typed scalar parameter in an
explicit pointy block signature, including when a hand-built tree is lowered
back through the existing regex matcher. The constructed callable continues to
enforce its declared type instead of losing the constraint during lowering.
