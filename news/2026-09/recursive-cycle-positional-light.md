---
title: Recursive positional-light calls no longer recurse through cyclic objects
category: fix
---

The positional-light call path no longer structurally compares cyclic objects
while tracking loop-local declarations. Recursive subs that build reference
cycles now return normally instead of hanging or overflowing the Rust stack.
