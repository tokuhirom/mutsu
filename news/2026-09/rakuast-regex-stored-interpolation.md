---
title: Preserve angle scalar regex interpolation in RakuAST
---

`/<$name>/` now retains Rakudo's `RakuAST::Regex::Assertion::InterpolatedVar`
shape, including sequential-branch context. Its value-sensitive execution
continues through mutsu's existing runtime regex parser and observes lexical
reassignment without entering the source-tree execution-plan cache. Constructed
RakuAST nodes also lower back through the normal Parser -> Compiler -> VM path.
