# A routine's `my regex` is its `&name`, even under an outer `my &name`

`my &r = sub { "outer" }; sub mk() { my regex r { x }; &r }` returned the outer `Sub` instead of the
routine's own regex (#17878). `&r` is a by-name read, and the routine's frame met the program
scope's `my &r` before it reached the registered regex. A routine that returned its regex for use
elsewhere (`/<&r>/`) then ran the wrong body: a second routine's `&r` was the first one's regex.

A `my sub` declared in the running code already answered its own `&name` ahead of the env; a
`regex`, `token` or `rule` declared there now does too.
