# A `sub` declared in a paren group or `$( ... )` stays in the enclosing scope

`my \x = $( sub foo { 1 }; 2 ); foo()` and `my $y = ( sub bar { 2 }; 3 ); bar()` died with
"Undeclared routine": the group's statements were run as a block that rolled its routines out of
the registry on exit. Item context and a parenthesised statement group open no scope in Raku, so a
routine (or user operator) declared in one is now visible afterwards; a real `do { }` block, and
the interpolation block of a string, still scope theirs. This was the reason Jupyter::Kernel's
sandbox lost an operator or sub declared by a `:store` evaluation (its code is wrapped in
`$( ... )`). (#17087)
