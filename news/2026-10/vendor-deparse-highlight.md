# `RakuAST::Deparse::Highlight` is vendored

`vendor/rakudo/` now also holds rakudo's `lib/RakuAST/Deparse/Highlight.rakumod` and
`lib/RakuAST/Deparse/Highlight/CORE.rakumod` at the pinned commit `9a94801c` (the same one `Deparse.rakumod` comes
from), unedited, as ADR-14845 and BATTERIES.md rung 2 require. It does not load yet: the module subclasses the Raku
compiler's own `Actions` and `Grammar` (`nqp::gethllsym('Raku','Actions')`), which mutsu does not expose
(`'Actions' cannot inherit from 'RakuActions' because it is unknown`). That is the slang-by-execution work of #15762;
#15793 stays open for the loading step.
