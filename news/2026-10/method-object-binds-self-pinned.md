# Invoking a looked-up Method object binds `self`: pinned

`Bar.^lookup("one")($b)` for a method that reads `$!one` used to die with
"Variable $!one used where no 'self' is available", and an auto-generated
accessor's Method object returned `Nil` (#10083). The Method-object
candidate-binding work (#10344 and its follow-ups) fixed both. A user method
called through `.^lookup` / `.^find_method` now reads its attributes and runs
exactly the looked-up candidate, even on a subclass instance that overrides
it. `t/oo/method/method-object-binds-self.t` now pins that, which no test did
until now.
