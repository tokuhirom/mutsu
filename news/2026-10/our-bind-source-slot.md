# Keep OUR bindings on their resolved source slot

Binding through `OUR::` now updates the source lexical's compiler-resolved slot. A later declaration with the same name in a nested package no longer redirects the binding to a sibling slot or violates the VM's env/slot invariant.
