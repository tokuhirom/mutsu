# Typed container attribute element checks remember a passing class

Assigning a hash or array to a typed container attribute
(`has Trieish %.children; ... %!children = %ch`) type-checked every element on
its own, and each check resolved the constraint name again (a role constraint
costs ~5k Ir). For an `Instance` element and a constraint that looks at nothing
but the type, the verdict is a function of the instance's class, so a class that
passed is remembered for the rest of that one loop (`ElemTypeMemo`); only a
pass is remembered, and never across assignments, so the frame and registry the
check resolves against cannot change under it.

`ML::TriesWithFrequencies` `trie-create`, 400 words, callgrind warm (profiling
build, whole run): 1,313.1M -> 1,289.8M Ir (-1.8%). The previous slice in the
same session (presence checks on container metadata no longer clone the type
names) was -0.5%. See #15235.
