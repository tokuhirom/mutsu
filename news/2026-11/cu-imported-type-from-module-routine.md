# `$*CU` built by a module routine resolves imported types

When the first read of `$*CU` happened inside a routine of a module (an analyzer
library called from a `CHECK`), a type the unit had imported was left unresolved,
because the type probe asked the scope of that routine. It now also consults the
main unit's imported types, so the tree matches rakudo's
(`RakuAST::Declaration::Import`). Fixes #18117.
