# "Actually thrown at:" only when it adds frames

An uncaught error from an unhandled `Failure` always printed a second
backtrace, "Actually thrown at:", for the place where the Failure was used.
`sub ff { fail "f" }; ff()` therefore listed `in block <unit>` twice. rakudo's
top-level handler prints the second backtrace only when the first does not
already end with it. A Failure sunk in the routine that made it, or in one of
its callers, now shows one backtrace, as on rakudo. A Failure used in
another routine still shows both.
