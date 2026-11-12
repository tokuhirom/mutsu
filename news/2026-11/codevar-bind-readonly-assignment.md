Value-bound code variables now raise `X::Assignment::RO` when assigned through `&name`, including from a closure. Code variables initialized with `=` remain writable.
