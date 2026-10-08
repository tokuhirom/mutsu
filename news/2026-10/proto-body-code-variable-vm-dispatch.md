# Code-variable calls keep non-trivial proto bodies in the VM

Calling a non-trivial proto through a code variable could tree-walk its body. That path merged a
same-named `my` lexical back into the caller's environment, so a nested method call could change
the caller's `$self`. Code-variable calls now run eligible proto bodies through the VM, where the
body's lexical stays local (#11920).
