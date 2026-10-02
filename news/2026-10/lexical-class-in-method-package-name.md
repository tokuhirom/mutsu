# A `my class` declared in a method is named under its enclosing class

`class Q { method b { my class X {}; X.^name } }` now reports `Q::X` like Rakudo
(it was `X`). Method dispatch only anchored `current_package` to the owner class
in a few cases, so a nested type declaration in a method of an otherwise plain
class registered under the bare name. The registry now records the classes with
a method that declares a class or role, and dispatch anchors those too.
Closes #10521.
