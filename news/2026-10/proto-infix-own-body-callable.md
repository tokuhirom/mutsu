# A `proto sub infix:<op>` with its own body is callable as an operator

`proto sub infix:<foo>($a, $b) { 42 }; say 1 foo 2` died with "Two terms in a row"
because neither the direct routine lookup nor the by-name call of the infix fallback
chain resolves a proto. A proto whose body does not dispatch (`{*}`) is an ordinary
routine, so the fallback now runs it through `call_proto_function`, as the plain-sub
spelling already did (mutsu#10696).
