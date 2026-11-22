# A routine signature's `ACCEPTS` runs `where` in the routine's own scope

`\(2) ~~ &closure.signature` checked the closure's `where` clause in the scope
of the code doing the smartmatch. A clause that read a variable the closure had
captured (`sub mk { my $lim = 3; sub (Int $x where * > $lim) { 1 } }`) saw
that variable undefined there, so every capture matched (#18661). Rakudo's trial
bind runs the clause where the routine was declared.

A routine with a body now keeps the closure as its signature's binder. A
capture is dry-run bound to the routine's parameters inside the closure's
captured scope, and the body is never run. The signature is still one object
per declaration, as in Rakudo. Each `.signature` read points it at the closure
it was read from. A signature literal still binds by calling its empty routine
(#18570).
