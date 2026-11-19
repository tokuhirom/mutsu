# A module used after `Test` runs its END before Test's, as in rakudo

ENDs run in reverse install order, and rakudo installs every `use` at compile
time, in source order. mutsu's compile-time loader loads most modules while
the main unit is still being parsed. It leaves `use Test`, and a `use` with
`:if` or an argument list, to the run-time `UseModule`. Module ENDs were
ordered by when they registered. So with `use Test; use M;`, M's END ran
after Test's instead of before it (#18183).

Green showed it: its END waits for the tests its worker thread runs, and
Test's END checks the plan. Test's ran first and reported
`# You planned 1 test, but ran 0`.

The loader now reserves an END position when it leaves a main-unit `use` to
run time, and the run-time load installs that module's ENDs there. With this
fix, #18135 and #18136, all four of Green's test files match raku.
