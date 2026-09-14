---
title: Preserve qualified regex subrules in RakuAST
---

Qualified regex subrules such as `<G::foo>` and `<.G::foo>` now retain their
segmented `RakuAST::Name.from-identifier-parts(...)` shape. Short aliases may
target qualified subrules, and constructed RakuAST trees continue through the
existing package-aware Parser -> Compiler -> VM matcher path.
