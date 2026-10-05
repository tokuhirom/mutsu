# Collection transforms use built-in method rows

`flat`, `sort`, `unique` and `repeated` now dispatch through the built-in method table for the
receiver shapes their Rakudo owners declare. The method table and native cascade share each
implementation. Comparator calls and other unsupported shapes keep their existing dispatch path.

The focused collection-transform method-row test and the corresponding `S32-list` roast files pass.
