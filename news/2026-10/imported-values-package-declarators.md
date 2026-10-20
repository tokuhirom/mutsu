# Package declaration syntax precedes imported value terms

Class, role and grammar expressions retain their declaration syntax when a
loaded module contributes a constant with the same name. Primary parsing
recognizes explicit declared terms first, then package declarations, then
imported value terms. Genuine categorical terms still override syntax, while
ordinary imported values can still shadow core constants.

This fixes the anonymous role passed to `.^mixin` in AttrX::Mooish's custom-HOW
test, where CustomHOW's `EXPORTHOW::SUPERSEDE` constants previously made the
parser stop before the role body.

The parser also retains its imported-value choice through RakuAST conversion
and lowering. An imported `e` remains a lexical value lookup after a round
trip instead of being replaced with the setting's mathematical constant;
ordinary setting terms outside the import's scope retain their meaning.
Module scans collect possible hook re-exports separately from the lexical
scope, so popping an import's block does not lose re-export candidates or
leave its value names visible in the enclosing program.
