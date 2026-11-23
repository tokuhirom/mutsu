# A failed compile still runs the END phasers of modules it loaded

Since ADR-17558, a `use`d module is loaded while the program is being parsed,
and it registers its END phasers then. rakudo runs those END phasers when the
compile fails after that point. `use EndOrderLate; 1 +;` prints the
`===SORRY!===` report and then the module's `EndOrderLate END`, under `-c`
too. mutsu returned the parse error from `run_unit` without calling `finish()`,
so the END phasers never ran.

Now the parse error is reported first (through the same early-report hook a
runtime error uses), and then the END phasers run. The exit status is still the
compile failure's. The mainline was never entered, so its LEAVE phasers are
skipped (`finish_unit(false)`).

Two related compile-time differences from rakudo remain and are filed as
#18921. A `BEGIN` that calls an imported routine does not run before a later
syntax error, and a clean `-c` compile runs a loaded module's END.
