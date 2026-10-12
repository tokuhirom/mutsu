# `<::(EXPR)>` calls run as frames, one end at a time

A symbolic subrule call (`<::('r')>`, `<::($name)>`) computed every end of its
rule before the caller entered the first, so a code block in the callee ran at
ends the match never reached (`regex r { a+ { @log.push: $/.to } }` over `aax`
logged `2,1` where rakudo logs `2`). The compiled engine now evaluates the name
where the cursor reaches the call and resolves it like `<name>`: a plain rule
or a proto runs as a frame whose Match is filed under the computed name, and
the other shapes (method, builtin, eager) take the same paths as a written call
(#7548).
