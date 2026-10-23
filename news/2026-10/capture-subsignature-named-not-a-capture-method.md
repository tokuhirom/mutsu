# A capture sub-signature's absent named parameter is not a Capture method

Binding `|c (:$base, Bool :$cache = True)` for a call that did not pass `:cache`
looked the name up as a method of the Capture when its named map had no such
entry, and `Capture.cache` answered a List: "Type check failed for cache:
expected Bool, got List". A Capture or Hash carries its named entries in its
map, so a name it does not hold is now simply absent and the parameter takes its
default. Red's `MetamodelX::Red::Model.alias(|c (..., Bool :$cache = True))`
hit it on every relationship join (#14094).
