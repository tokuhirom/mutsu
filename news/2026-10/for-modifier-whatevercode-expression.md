# `(* + 1 for @a)` builds one WhateverCode per element

A statement-modifier `for` no longer treats a Whatever-curried expression as the
loop's block. `* + 1 for @a` evaluates the expression once per iteration and
yields a WhateverCode per element, like Rakudo; a pointy block is still called
per element (#13308).
