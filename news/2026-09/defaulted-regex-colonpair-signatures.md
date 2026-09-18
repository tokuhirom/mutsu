---
title: Defaulted regex colonpair signatures retain their defaults
category: fix
---

RakuAST regex arguments now preserve a simple defaulted scalar parameter in an
explicit pointy block signature. Constructed callables continue to supply the
declared default when the regex matcher invokes them without that argument.
