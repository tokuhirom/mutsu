# Package declaration syntax precedes imported value terms

Class, role and grammar expressions retain their declaration syntax when a
loaded module contributes a constant with the same name. Primary parsing
recognizes explicit declared terms first, then package declarations, then
imported value terms. Genuine categorical terms still override syntax, while
ordinary imported values can still shadow core constants.

This fixes the anonymous role passed to `.^mixin` in AttrX::Mooish's custom-HOW
test, where CustomHOW's `EXPORTHOW::SUPERSEDE` constants previously made the
parser stop before the role body.
