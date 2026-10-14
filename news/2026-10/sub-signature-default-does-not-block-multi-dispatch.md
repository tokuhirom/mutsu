# A defaulted sub-signature parameter no longer blocks multi dispatch

Matching a multi candidate against an argument that is destructured by a sub-signature
(`:$referencing! (Str :$model!, Str :$require = $model)`) evaluated the omitted parameter's default
while dispatching, where the earlier parameter it refers to is not bound yet, and then rejected the
candidate on the resulting type check. An omitted parameter with a default now always matches, as in
Rakudo; the default is evaluated at bind time. Found with the Red distribution (`is referencing`).
