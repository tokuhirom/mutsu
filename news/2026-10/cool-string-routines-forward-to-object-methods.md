# lc($obj), substr($obj, ...) and friends call the object's own method

Rakudo defines the `Cool` string routines as `sub lc(\x) { x.lc }`, so a class that declares (or composes a
role that declares) its own `lc`/`uc`/`flip`/`chomp`/`ord`/`substr`/... answers the routine form too. The
native routine arms stringified the argument instead, bypassing the method: Red's `lc .email` index key
became a lowercased `Str` rather than a `Lowercase` AST node. The routine form now forwards to the method
when the first argument is an object whose class has one.
