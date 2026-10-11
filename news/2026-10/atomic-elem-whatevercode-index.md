# Element atomics accept a WhateverCode index

`cas(@a[*-1], ...)`, the code form of `cas`, and `atomic-fetch-inc(@a[*-1])` and its siblings
used to parse a `WhateverCode` index to `0` and silently worked on the first element. The index is
now resolved against the container's length (`Interpreter::atomic_elem_index`), shared by all three
element-atomic builtins. Closes #12695.
