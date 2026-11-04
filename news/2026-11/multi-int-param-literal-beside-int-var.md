# A literal beside a native argument binds a native `int` parameter of a multi

`multi f(int, int)` called as `f 42, my int $x` died "Cannot resolve caller
f(Int:D, Int:D)". Two things were missing: a named `my int $x` declaration used
as an argument was not recognised as a genuinely native argument (only the
anonymous form was), and a boxed literal never bound a native parameter even
when another argument of the call was native. rakudo's native-aware bind unboxes
the literal in that case (`f 7, $native`, `f $native, 7`) but still refuses two
literals (`f 7, 7`) and a plain `Int` variable beside a literal.
