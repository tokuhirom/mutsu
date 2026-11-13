# A `--> Bool` routine may return a Bool read out of a hash

A Bool stored into a real Hash element is itemized, and the light call path's return-type check matched the item rather than what it holds, so `sub f(%h --> Bool) { %h<k> }` died with "Type check failed for return value; expected Bool but got Bool (False)". The parameter check and the by-name return check already looked through the item; the tagged return check now does too, and, like the parameter check, lets a Bool (a value or the type object) satisfy an `Int` return type, as `Bool` is-an `Int`.

EEL's `EEL::Runner::nullable` returns `%nullable{$node.name} // False` from a `--> Bool` sub, which every runner construction reached, so most of EEL's test files died at their first runner. With this fix every EEL 0.3 test file that passes under rakudo passes under mutsu too (t/06-runner 83/83, t/34-per-event-selection 161/161, ...); `t/28` and `t/29` take ~4x rakudo's time (#17530).
