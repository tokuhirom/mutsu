# Method traits reach `trait_mod:<is>` on proto and role methods

A user `trait_mod:<is>(Method ...)` handler used to run only for plain class
methods. `proto method` declarations dropped their trait arguments at parse
time, and role methods and role proto methods never dispatched their traits at
all. All of them now dispatch once, at declaration, with the trait argument and
with `$*PACKAGE` bound to the declaring class or role (found while working the
`Method::Also` distribution, whose `is also<...>` on a role proto method still
needs the role-HOW `specialize` hook).
