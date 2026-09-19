---
title: Named regex colonpair signatures retain their binding
category: fix
---

RakuAST regex arguments now preserve an ordinary named scalar parameter in an
explicit pointy block signature. Constructed callables bind the parameter by
name, and constructed regex arguments retain the same `:$name` source form.
