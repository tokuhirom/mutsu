# A native-only multi does not accept a boxed literal

`multi n(byte $a) { }` called as `n(1)` dispatched, while rakudo 2026.09 treats a
literal as a boxed `Int` and refuses ("will never work"). The call-site literal
mask no longer lets a literal satisfy a native multi parameter; a native-typed
variable still does, a plain `sub f(byte $a)` still accepts `f(1)`, and beside a
boxed candidate the boxed one wins as before.
